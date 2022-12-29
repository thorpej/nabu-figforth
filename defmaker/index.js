
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

const makeAssemblerDefinitions = (stream, prefix, definitions) => {
    definitions.forEach(({ direction, messageName, fields, messageType }) => {
        const tag = prefix + direction + '_' + messageName.replace(/-/g, '_',) + ':'
        printf(stream, "%-33s .equ 0%02xh\n", tag, messageType)
    })
}

const makePythonDefinitions = (stream, prefix, definitions) => {
    definitions.forEach(({ direction, messageName, fields, messageType }) => {
        const tag = prefix + direction + '_' + messageName.replace(/-/g, '_',)
        printf(stream, "%-33s = 0x%02x\n", tag, messageType)
    })
}

console.log("\nAssembler definitions\n")
makeAssemblerDefinitions(process.stdout, 'NHACP_', messageDefinitions)

console.log("\nPython definitions\n")
makePythonDefinitions(process.stdout, 'NHACP_', messageDefinitions)
