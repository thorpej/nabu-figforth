
const marked = require('marked')
const fs = require('fs')
const path = require('path')
const printf = require('printf')
const trim = (string) => string.replace(/^\s*(.*?)\s*$/, "$1")

const messageDefinitions = []
const parser = {
    messageType: null,
    currentDefinition: null,

    heading(text, level) {
        // The markdown parser behaves somewhat strangely in that it sometimes seems to set this.currentDefinition
        // to unexpected values.  I don't have time for doing this better right now.
        if (typeof this.currentDefinition === 'object' && this.currentDefinition?.messageName) {
            messageDefinitions.push(this.currentDefinition)
            this.currentDefinition = null
        }
        switch (level) {
            case 2:
                if (text.match(/request messages/i)) {
                    this.messageType = 'REQ'
                } else if (text.match(/response messages/i)) {
                    this.messageType = 'RES'
                }
                break
            case 3:
                if (this.messageType) {
                    this.currentDefinition = {
                        direction: this.messageType,
                        messageName:  trim(text),
                        fields: []
                    }
                }
                break
        }
    },

    tablerow(header, body) {
        this.colIndex = 0
    },

    tablecell(content, { header }) {
        if (this.currentDefinition && !header) {
            switch (this.colIndex) {
                case 0:
                    this.fieldName = content
                    break
                case 1:
                    this.currentDefinition.fields.push({ name: this.fieldName, type: content})
                    break;
                case 2:
                    if (this.fieldName === 'type') {
                        this.currentDefinition.messageType = parseInt(content)
                    }
            }
            this.colIndex++
        }
    }
}

const codeGenerators = {
    asm: (stream, definitions) => {
        const MAX_LABEL_LENGTH = 31
        const defineAsm = (label, directive, value) => {
            if (label) {
                label = label.substring(0, MAX_LABEL_LENGTH) + ':'
            }
            printf(stream, "%-33s  %-8s  ", label || '', directive || '')
            if (typeof value === 'number') {
                if (value > 255) {
                    printf(stream, "0%04xh\n", value)
                } else {
                    printf(stream, "0%02xh\n", value)
                }
            } else {
                printf(stream, "%s\n", value || '')
            }
        }
        defineAsm('NHACP_START', '.equ', '0AFH')
        printf(stream, '\n')
        const makeTypeTagName = (direction, messageName) => 'NHACP_' + direction + '_' + messageName.replace(/-/g, '_',)
        const parseTypeSize = type => {
                const match = type.match(/char\[(\d+)\]/)
                if (match) {
                    return match[1]
                }
                switch (type) {
                    case 'u8': return 1
                    case 'u16': return 2
                    case 'u32': return 4
                    default: console.log('unknown data type ' + type)
                }

        }
        definitions.forEach(({ direction, messageName, fields, messageType }) => {
            const tag = makeTypeTagName(direction, messageName)
            defineAsm(tag, '.equ', messageType)
        })
        const toAsmName = s => s.replace(/-/g, '_').toLowerCase()
        // Define message layouts
        let previousDirection
        definitions.forEach(({ direction, messageName, fields, messageType }) => {
            if (direction !== previousDirection && direction === 'RES') {
                printf(stream, '\n;;; ---------- Response buffers\n\n')
            }
            previousDirection = direction
            printf(stream, "\n;;; %s-%s\n", direction, messageName)
            const blockName = toAsmName(messageName + '_' + direction)
            if (direction === 'REQ') {
                defineAsm(null, '.word', '0')
            } else if (direction === 'RES') {
                defineAsm(null, '.byte', `${blockName}_length`)
            }
            defineAsm(blockName, '.byte', makeTypeTagName(direction, messageName))
            fields.slice(1).forEach(({name, type}) => {
                if (!type.match(/\*$/)) {
                    defineAsm(blockName + '.' + toAsmName(name), '.ds', parseTypeSize(type))
                }
            })
            defineAsm(`${blockName}_length`, '.equ', `$ - ${blockName}`)
        })
        // Define response message dispatch table
        printf(stream, '\n;;; ---------- Response message dispatch table\n\n')
        defineAsm('response_message_dispatch_table')
        let previousMessageType
        let maxResponseMessageType
        definitions.forEach(({ direction, messageName, fields, messageType }) => {
            if (direction === 'RES') {
                if (previousMessageType && previousMessageType !== (messageType - 1)) {
                    console.error('non-consequitive response message', messageName)
                    process.exit(1)
                }
                previousMessageType = messageType
                const blockName = toAsmName(messageName + '_' + direction)
                defineAsm(null, '.db', `${blockName}_length - 1`)
                defineAsm(null, '.dw', `${blockName} + 1`)
                defineAsm(null, '.db', 0)
                maxResponseMessageType = messageType
            }
        })
        defineAsm('response_type_count', '.equ', (maxResponseMessageType & 0x7f) + 1)
        printf(stream, "\n")
    },
    py: (stream, definitions) => {
        definitions.forEach(({direction, messageName, fields, messageType}) => {
            const tag = 'NHACP_' + direction + '_' + messageName.replace(/-/g, '_',)
            printf(stream, "%-33s = 0x%02x\n", tag, messageType)
        })
    },
    lisp: (stream, definitions) => {
        printf(stream, ";; generated -*- Lisp -*- code, do not edit\n\n")
        printf(stream, "(in-package :nhacp)\n\n")
        printf(stream, "(defconstant +NHACP-START+ #xAF)\n\n")
        printf(stream, "(defvar *type-tag-to-name* (make-hash-table))\n\n")
        const makeConstantName = (direction, messageName) => '+NHACP-' + direction + '-' + messageName + '+'
        definitions.forEach(({direction, messageName, fields, messageType}) => {
            printf(stream, "(defconstant %-33s #x%02x)\n", makeConstantName(direction, messageName), messageType)
        })
        printf(stream, "\n")
        printf(stream, "(define-unsigned u8 1 :little-endian)\n")
        printf(stream, "(define-unsigned u16 2 :little-endian)\n")
        printf(stream, "(define-unsigned u32 4 :little-endian)\n")
        printf(stream, "\n")

        printf(
            stream,
            "(defclass nhacp-message ()\n" +
            "  ((type-tag :allocation :class :initarg :type-tag)))\n\n")

        definitions.forEach(({direction, messageName, fields, messageType}) => {
            printf(
                stream,
                "(define-binary-class %s-%s (nhacp-message)\n  (",
                messageName.toLowerCase(),
                direction === 'REQ' ? 'request' : 'response')
            fields.slice(1).forEach(({name, type}, i) => {
                if (!type.match(/\*$/)) {
                    if (i !== 0) {
                        printf(stream, "\n   ")
                    }
                    const match = type.match(/char\[(\d+)\]/)
                    if (match) {
                        const length = match[1]
                        printf(stream, "(%s :initarg :%s :binary-type (define-binary-string nhacp-%s-string %d))", name, name, name, length)
                    } else {
                        printf(stream, "(%s :initarg :%s :binary-type %s)", name, name, type)
                    }
                }
            })
            const constantName = makeConstantName(direction, messageName)
            printf(stream, ")\n")
            printf(stream, "  (:default-initargs :type-tag %s))\n", constantName)
            printf(stream, '\n(setf (gethash %s *type-tag-to-name*) "%s")\n\n', constantName, direction + '-' + messageName)
        })
    }
}

const makeDefinitions = (stream, language, definitions) => {
    codeGenerators[language](stream, definitions)
}

const inputFile = process.argv[2]
marked.use({ renderer: parser })
marked.parse(fs.readFileSync(inputFile, 'utf-8'))

for (const [language] of Object.entries(codeGenerators)) {
    const outputFilename = path.dirname(inputFile) + path.sep + path.basename(inputFile, ".md") + "." + language
    console.log('Generating', outputFilename)
    const output = fs.createWriteStream(outputFilename)
    makeDefinitions(output, language, messageDefinitions)
}
