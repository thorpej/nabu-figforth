
const marked = require('marked')
const fs = require('fs')
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
marked.use({ renderer: parser })
marked.parse(fs.readFileSync(process.argv[2], 'utf-8'))

const codeGenerators = {
    python: (stream, definitions) => {
        definitions.forEach(({ direction, messageName, fields, messageType }) => {
            const tag = 'NHACP_' + direction + '_' + messageName.replace(/-/g, '_',) + ':'
            printf(stream, "%-33s .equ 0%02xh\n", tag, messageType)
        })
    },
    assembler: (stream, definitions) => {
        definitions.forEach(({direction, messageName, fields, messageType}) => {
            const tag = 'NHACP_' + direction + '_' + messageName.replace(/-/g, '_',)
            printf(stream, "%-33s = 0x%02x\n", tag, messageType)
        })
    },
    lisp: (stream, definitions) => {
        definitions.forEach(({direction, messageName, fields, messageType}) => {
            const name = '*NHACP-' + direction + '-' + messageName + '*'
            printf(stream, "(defconstant %-33s #x%02x)\n", name, messageType)
        })
    }
}

const makeDefinitions = (language, definitions) => {
    console.log("\nGenerating definitions for", language, "\n")
    codeGenerators[language](process.stdout, definitions)
}
makeDefinitions('python', messageDefinitions)
makeDefinitions('assembler', messageDefinitions)
makeDefinitions('lisp', messageDefinitions)
