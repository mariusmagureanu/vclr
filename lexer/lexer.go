package lexer

import (
	_ "fmt"

	"github.com/varnish/vclr/token"
)

var (
	durations = []byte{'s', 'm', 'h', 'y', 'd', 'w'}
)

type Lexer struct {
	input        string
	position     int
	readPosition int
	ch           byte
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()

	return l
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()

	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			tok = newToken(token.ASSIGN, l.ch)
		}
	case '&':
		if l.peekChar() == '&' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.LOGICAL_AND, Literal: literal}
		} else {
			tok = newToken(token.AMPERSAND, l.ch)
		}
	case '|':
		if l.peekChar() == '|' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.LOGICAL_OR, Literal: literal}
		} else {
			tok = newToken(token.PIPE, l.ch)
		}
	// when reading comments we advance here, no reason
	// to offload this to the parser. Instead, the parser
	// will receive a comment token with the commented text
	// as its literal value.
	case '#':
		tok.Type = token.COMMENT_ONE
		tok.Literal = l.readStringToEOL()
	case '/':
		if l.peekChar() == '/' {
			l.readChar()
			tok.Type = token.COMMENT_TWO
			tok.Literal = l.readStringToEOL()
		} else if l.peekChar() == '*' {
			l.readChar()
			tok.Type = token.START_COMMENT
			tok.Literal = l.readStringToEndOfCommentBlock()

		} else {
			tok = newToken(token.SLASH, l.ch)
		}
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '>':
		tok = newToken(token.GT, l.ch)
	case '<':
		tok = newToken(token.LT, l.ch)
	case '~':
		tok = newToken(token.MATCH, l.ch)
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case '*':
		if l.peekChar() == '/' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.END_COMMENT, Literal: literal}
		} else {
			tok = newToken(token.ASTERISK, l.ch)
		}
	case '"':
		tok.Type = token.STRING
		tok.Literal = l.readString()
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case '!':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else if l.peekChar() == '~' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_MATCH, Literal: literal}
		} else {
			tok = newToken(token.BANG, l.ch)
		}

	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.ch) {
			if l.peekChar() == '.' {
				ch := l.ch
				l.readChar()
				literal := string(ch) + string(l.ch)
				l.readChar()
				afterDot := l.readInteger()

				tok.Literal = literal + afterDot
				tok.Type = token.REAL

			} else {
				tok.Type = token.INT
				tok.Literal = l.readInteger()
			}

			if isDurationChar(l.ch) {
				tok.Type = token.DURATION
				tok.Literal = tok.Literal + string(l.ch)
				l.readChar()
			}

			return tok
		} else {
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar()
	return tok
}

func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}

	l.position = l.readPosition

	l.readPosition += 1
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}

	return l.input[position:l.position]

}

func (l *Lexer) readString() string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 {
			break
		}
	}

	return l.input[position:l.position]
}

func (l *Lexer) readStringToEOL() string {
	position := l.position + 1

	for {
		l.readChar()
		if l.ch == '\n' || l.ch == 0 {
			break
		}
	}

	return l.input[position:l.position]
}

// readStringToEndOfCommentBlock reads until we
// start meeting '*/' but without
// actually advancing on those characters, we're
// breaking out when we're about the meet them.
func (l *Lexer) readStringToEndOfCommentBlock() string {
	position := l.position + 1

	for {
		l.readChar()

		if l.ch == 0 {
			break
		}

		// check there's still room to read
		if l.readPosition+3 > len(l.input) {
			break
		}

		x := l.input[l.readPosition+1]
		y := l.input[l.readPosition+2]

		if x == '*' && y == '/' {
			break
		}

	}

	return l.input[position:l.position]
}

func (l *Lexer) readInteger() string {
	position := l.position

	for isDigit(l.ch) {
		l.readChar()
	}

	return l.input[position:l.position]
}

func isDurationChar(ch byte) bool {

	for _, c := range durations {
		if string(c) == string(ch) {
			return true
		}
	}

	return false
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_' || ch == '.' || ch == '-'
}
