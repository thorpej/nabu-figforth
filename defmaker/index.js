
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
            printf(stream, "%-33s  %-8s  %s\n", label.substring(0, MAX_LABEL_LENGTH) + ':', directive, value)
        }
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
        definitions.forEach(({ direction, messageName, fields, messageType }) => {
            printf(stream, "\n;;; %s-%s\n", direction, messageName)
            const toAsmName = s => s.replace(/-/g, '_').toLowerCase()
            const blockName = toAsmName(messageName + '_' + direction)
            defineAsm(blockName, '.byte', makeTypeTagName(direction, messageName))
            fields.slice(1).forEach(({name, type}) => {
                if (!type.match(/\*$/)) {
                    defineAsm(blockName + '.' + toAsmName(name), '.ds', parseTypeSize(type))
                }
            })
            defineAsm(`${blockName}_length`, '.equ', `$ - ${blockName}`)
        })
        printf(stream, "\n")
    },
    py: (stream, definitions) => {
        definitions.forEach(({direction, messageName, fields, messageType}) => {
            const tag = 'NHACP_' + direction + '_' + messageName.replace(/-/g, '_',)
            printf(stream, "%-33s = 0x%02x\n", tag, messageType)
        })
    },
    lisp: (stream, definitions) => {
        printf(stream, "(defpackage :nhacp-defs (:use :cl))\n\n")
        printf(stream, "(in-package :nhacp-defs)\n\n")
        const makeConstantName = (direction, messageName) => '+NHACP-' + direction + '-' + messageName + '+'
        definitions.forEach(({direction, messageName, fields, messageType}) => {
            printf(stream, "(defconstant %-33s #x%02x)\n", makeConstantName(direction, messageName), messageType)
        })
        printf(stream, "\n")
        printf(stream, "(binary-types:define-unsigned u8 1 :little-endian)\n")
        printf(stream, "(binary-types:define-unsigned u16 2 :little-endian)\n")
        printf(stream, "(binary-types:define-unsigned u32 4 :little-endian)\n")
        printf(stream, "\n")

        printf(
            stream,
            "(defclass nhacp-message ()\n" +
            "  ((type-tag :allocation :class :initarg :type-tag)))\n\n")

        definitions.forEach(({direction, messageName, fields, messageType}) => {
            if (fields.length > 1) {
                printf(
                    stream,
                    "(binary-types:define-binary-class %s-%s (nhacp-message)\n  (",
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
                            printf(stream, "(%s :initarg :%s :binary-type (binary-types:define-binary-string nhacp-%s-string %d))", name, name, name, length)
                        } else {
                            printf(stream, "(%s :initarg :%s :binary-type %s)", name, name, type)
                        }
                    }
                })
                printf(stream, ")\n")
                printf(stream, "  (:default-initargs :type-tag %s))\n\n", makeConstantName(direction, messageName))
            }
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
