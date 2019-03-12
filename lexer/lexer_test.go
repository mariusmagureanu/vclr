package lexer

import (
	"testing"

	"github.com/varnish/vclr/token"
)

func TestNextTokenLite(t *testing.T) {
	input := `=4.1y,7,6,5.06,(0){},;==!=~!!~|||&&&>><`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.ASSIGN, "="},
		{token.DURATION, "4.1y"},
		{token.COMMA, ","},
		{token.INT, "7"},
		{token.COMMA, ","},
		{token.INT, "6"},
		{token.COMMA, ","},
		{token.REAL, "5.06"},
		{token.COMMA, ","},
		{token.LPAREN, "("},
		{token.INT, "0"},
		{token.RPAREN, ")"},
		{token.LBRACE, "{"},
		{token.RBRACE, "}"},
		{token.COMMA, ","},
		{token.SEMICOLON, ";"},
		{token.EQ, "=="},
		{token.NOT_EQ, "!="},
		{token.MATCH, "~"},
		{token.BANG, "!"},
		{token.NOT_MATCH, "!~"},
		{token.LOGICAL_OR, "||"},
		{token.PIPE, "|"},
		{token.LOGICAL_AND, "&&"},
		{token.AMPERSAND, "&"},
		{token.GT, ">"},
		{token.GT, ">"},
		{token.LT, "<"},
		{token.EOF, ""},
	}

	l := New(input)

	for i, tt := range tests {
		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - token type wrong, expected %q, got %q", i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong, expected %q, got %q", i, tt.expectedLiteral, tok.Literal)
		}
	}
}
