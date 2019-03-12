package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/varnish/vclr/ast"
	"github.com/varnish/vclr/lexer"
	"github.com/varnish/vclr/token"
)

const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     //*
	PREFIX      // -X or !X
	CALL
)

var (
	variables = map[string][]string{
		"req.method": []string{"vcl_recv", "vcl_pass"},
		"req.url":    []string{"vcl_recv", "vcl_pass"},
		"req.hash":   []string{"vcl_pass"},
		"req.http.*": []string{"vcl_recv", "vcl_pass", "vcl_backend_fetch"},
	}

	precedences = map[token.TokenType]int{
		token.ASSIGN:      EQUALS,
		token.EQ:          EQUALS,
		token.NOT_EQ:      EQUALS,
		token.MATCH:       EQUALS,
		token.NOT_MATCH:   EQUALS,
		token.LOGICAL_AND: EQUALS,
		token.LOGICAL_OR:  EQUALS,
		token.LT:          LESSGREATER,
		token.GT:          LESSGREATER,
		token.PLUS:        SUM,
		token.MINUS:       SUM,
		token.SLASH:       PRODUCT,
		token.ASTERISK:    PRODUCT,
		token.LPAREN:      CALL,
	}
)

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	l *lexer.Lexer

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn

	currentToken token.Token
	peekToken    token.Token
	errors       []string

	currentSub string
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l,
		errors: []string{},
	}

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.infixParseFns = make(map[token.TokenType]infixParseFn)

	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.REAL, p.parseRealLiteral)
	p.registerPrefix(token.DURATION, p.parseDurationLiteral)
	p.registerPrefix(token.STRING, p.parseStringLiteral)
	p.registerPrefix(token.COMMENT_ONE, p.parseCommentStatement)
	p.registerPrefix(token.COMMENT_TWO, p.parseCommentStatement)
	p.registerPrefix(token.ACL, p.parseAclExpression)
	p.registerPrefix(token.BACKEND, p.parseBackendExpression)
	p.registerPrefix(token.PROBE, p.parseProbeExpression)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)
	p.registerPrefix(token.IF, p.parseIfExpression)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.START_COMMENT, p.parseCommentBlockStatement)

	p.registerInfix(token.LPAREN, p.parseCallExpression)
	p.registerInfix(token.ASSIGN, p.parseInfixExpression)
	p.registerInfix(token.MATCH, p.parseInfixExpression)
	p.registerInfix(token.NOT_MATCH, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LOGICAL_AND, p.parseInfixExpression)
	p.registerInfix(token.LOGICAL_OR, p.parseInfixExpression)

	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.currentToken.Type != token.EOF {
		stmt := p.parseStatement()

		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}

		p.nextToken()
	}

	return program
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
	}

	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
		Left:     left,
	}

	precedence := p.currentPrecedence()

	p.nextToken()

	expression.Right = p.parseExpression(precedence)

	return expression
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.currentToken.Type {
	case token.SET:
		return p.parseSetStatement()
	case token.VCL:
		return p.parseVclStatement()
	case token.INCLUDE:
		return p.parseIncludeStatement()
	case token.IMPORT:
		return p.parseImportStatement()
	case token.UNSET:
		return p.parseUnsetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	case token.CALL:
		return p.parseCallFunctionStatement()
	default:
		return p.parseExpressionStatement()

	}

	return nil
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.currentToken}

	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.currentToken.Type]

	if prefix == nil {
		p.prefixParserFnError(p.currentToken)
		return nil
	}

	leftExp := prefix()

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {

		infix := p.infixParseFns[p.peekToken.Type]

		if infix == nil {
			return leftExp
		}

		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.currentToken}

	p.nextToken()

	stmt.ReturnValue = p.parseGroupedExpression() //p.parseExpression(LOWEST)

	for !p.currentTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseCallArguments() []ast.Expression {
	args := []ast.Expression{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return args
	}

	p.nextToken()

	args = append(args, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()

		args = append(args, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return args
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.currentToken, Function: function}
	exp.Arguments = p.parseCallArguments()

	return exp
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)

	//p.nextToken() ?? need to look into this

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return exp
}

func (p *Parser) prefixParserFnError(t token.Token) {
	errMsg := fmt.Sprintf("no parsing function found for %s %s.", t.Type, t.Literal)
	p.errors = append(p.errors, errMsg)
}

func (p *Parser) currentTokenIs(t token.TokenType) bool {
	return p.currentToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) peekPrecedence() int {

	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}

	return LOWEST
}

func (p *Parser) currentPrecedence() int {
	if p, ok := precedences[p.currentToken.Type]; ok {
		return p
	}

	return LOWEST
}

func (p *Parser) peekError(t token.TokenType) {
	errMsg := fmt.Sprintf("Expected next token to be %s but got %s %s instead.", t, p.peekToken.Type, p.peekToken.Literal)
	p.errors = append(p.errors, errMsg)
}

func (p *Parser) nextToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.currentToken}

	value, err := strconv.ParseInt(p.currentToken.Literal, 0, 64)

	if err != nil {
		errMsg := fmt.Sprintf("Could not parse %q as integer.", p.currentToken.Literal)
		p.errors = append(p.errors, errMsg)
		return nil
	}

	lit.Value = value

	return lit
}

func (p *Parser) parseRealLiteral() ast.Expression {
	rl := &ast.RealLiteral{Token: p.currentToken}

	value, err := strconv.ParseFloat(p.currentToken.Literal, 64)

	if err != nil {
		errMsg := fmt.Sprintf("Could not parse %q as float.", p.currentToken.Literal)
		p.errors = append(p.errors, errMsg)
		return nil
	}

	rl.Value = value

	return rl
}

func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.currentToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	p.nextToken()

	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()

		if !p.expectPeek(token.LBRACE) {
			return nil
		}

		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

func (p *Parser) parseDurationLiteral() ast.Expression {
	dl := &ast.DurationLiteral{Token: p.currentToken, Value: p.currentToken.Literal}

	return dl
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.currentToken, Value: p.currentToken.Literal}
}

func (p *Parser) parseSetStatement() *ast.SetStatement {
	stmt := &ast.SetStatement{Token: p.currentToken}

	if !p.expectPeek(token.IDENT) {
		p.errors = append(p.errors, "Missing identifier in set statement.")
		return nil
	}

	if !p.isValidVariable(p.currentToken.Literal) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.expectPeek(token.ASSIGN) {
		p.errors = append(p.errors, "Missing assignment in set statement.")
		return nil
	}

	p.nextToken()

	stmt.Value = p.parseExpression(LOWEST)

	/*
		for !p.currentTokenIs(token.SEMICOLON) {
			p.nextToken()
		}
	*/

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseVclStatement() *ast.VclStatement {
	stmt := &ast.VclStatement{Token: p.currentToken}

	if !p.expectPeek(token.REAL) {
		return nil
	}

	v, err := strconv.ParseFloat(p.currentToken.Literal, 64)

	if err != nil {
		p.errors = append(p.errors, err.Error())
		return nil
	}

	stmt.Value = v

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt

}

func (p *Parser) parseIncludeStatement() *ast.IncludeStatement {
	stmt := &ast.IncludeStatement{Token: p.currentToken}

	p.nextToken()

	stmt.Value = p.currentToken.Literal

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseCallFunctionStatement() *ast.CallStatement {
	stmt := &ast.CallStatement{Token: p.currentToken}

	p.nextToken()

	stmt.Value = p.currentToken.Literal

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseUnsetStatement() *ast.UnsetStatement {
	stmt := &ast.UnsetStatement{Token: p.currentToken}

	p.nextToken()

	stmt.Value = p.currentToken.Literal

	if !p.isValidVariable(stmt.Value) {
		return nil
	}

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseImportStatement() *ast.ImportStatement {
	stmt := &ast.ImportStatement{Token: p.currentToken}

	p.nextToken()

	stmt.Value = p.currentToken.Literal

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) isValidVariable(vclVar string) bool {

	if sub, found := variables[vclVar]; found {
		for _, s := range sub {
			if s == p.currentSub {
				return true
			}
		}

		p.errors = append(p.errors, fmt.Sprintf("%s is not allowed in %s", vclVar, p.currentSub))
		return false
	}

	return true

}

func (p *Parser) parseIdentifier() ast.Expression {

	if !p.isValidVariable(p.currentToken.Literal) {
		return nil
	}

	return &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.currentToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
		stmt := p.parseStatement()

		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}

		p.nextToken()
	}

	if p.currentToken.Type != token.RBRACE {
		p.errors = append(p.errors, "Unclosed brace in  "+block.String())
	}
	return block
}

func (p *Parser) parseAclExpression() ast.Expression {
	acl := &ast.AclExpression{Token: p.currentToken}

	p.nextToken()

	acl.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	p.nextToken()

	acl.Body = p.parseBlockStatement()

	//TODO: we can get into detailed parsing here. ipv4, ipv6...etc

	return acl
}

func (p *Parser) parseBackendExpression() ast.Expression {
	backend := &ast.BackendExpression{Token: p.currentToken}

	p.nextToken()

	backend.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	p.nextToken()

	backend.Body = p.parseBlockStatement()

	return backend
}

func (p *Parser) parseProbeExpression() ast.Expression {
	probe := &ast.ProbeExpression{Token: p.currentToken}

	p.nextToken()

	probe.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	p.nextToken()

	probe.Body = p.parseBlockStatement()

	return probe
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	fn := &ast.FunctionLiteral{Token: p.currentToken}

	p.nextToken()

	fn.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if strings.HasPrefix(fn.Name.Value, "vcl_") {
		p.currentSub = fn.Name.Value
	}

	p.nextToken()

	fn.Body = p.parseBlockStatement()

	return fn
}

func (p *Parser) parseCommentStatement() ast.Expression {
	return &ast.CommentStatement{Token: p.currentToken, Value: p.currentToken.Literal}
}

func (p *Parser) parseCommentBlockStatement() ast.Expression {
	stmt := &ast.CommentBlockStatement{Token: p.currentToken, Value: p.currentToken.Literal}

	p.nextToken()

	if p.currentToken.Type != token.END_COMMENT {
		p.errors = append(p.errors, "Unclosed block comment in  "+stmt.String())
	}

	return stmt
}
