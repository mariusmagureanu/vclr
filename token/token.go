package token

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"

	//Identifiers and literals
	IDENT    = "IDENT"    // add, foo, bar, x, y...
	INT      = "INT"      // 1232
	REAL     = "REAL"     // 3.14
	DURATION = "DURATION" // 4.50s
	STRING   = "STRING"

	//Operators
	PLUS     = "+"
	MINUS    = "-"
	ASTERISK = "*"
	SLASH    = "/"

	BANG        = "!"
	ASSIGN      = "="
	MATCH       = "~"
	NOT_MATCH   = "!~"
	EQ          = "=="
	NOT_EQ      = "!="
	LOGICAL_OR  = "||"
	LOGICAL_AND = "&&"
	PIPE        = "|"
	AMPERSAND   = "&"

	COMMENT_ONE   = "#"
	COMMENT_TWO   = "//"
	START_COMMENT = "/*"
	END_COMMENT   = "*/"

	LT = "<"
	GT = ">"

	//Delimiters
	COMMA     = ","
	SEMICOLON = ";"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"

	// Keywords
	FUNCTION = "FUNCTION"
	SET      = "SET"
	UNSET    = "UNSET"
	TRUE     = "TRUE"
	FALSE    = "FALSE"
	IF       = "IF"
	ELSE     = "ELSE"
	ELSEIF   = "ELSEIF"
	RETURN   = "RETURN"
	INCLUDE  = "INCLUDE"
	VCL      = "VCL"
	IMPORT   = "IMPORT"
	ACL      = "ACL"
	BACKEND  = "BACKEND"
	PROBE    = "PROBE"
	CALL     = "CALL"
)

var keywords = map[string]TokenType{
	"call":    CALL,
	"probe":   PROBE,
	"backend": BACKEND,
	"acl":     ACL,
	"sub":     FUNCTION,
	"include": INCLUDE,
	"import":  IMPORT,
	"vcl":     VCL,
	"set":     SET,
	"unset":   UNSET,
	"true":    TRUE,
	"false":   FALSE,
	"if":      IF,
	"else":    ELSE,
	"elseif":  ELSEIF,
	"elif":    ELSEIF,
	"elsif":   ELSEIF,
	"return":  RETURN,
}

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}

	return IDENT
}
