package parser

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/varnish/vclr/ast"
	"github.com/varnish/vclr/lexer"
	"github.com/varnish/vclr/token"
)

const (
	// operator precedence order
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     //*
	PREFIX      // -X or !X
	CALL
)

func x() []string {
	return append(clientRoutines, backendRoutines...)
}

var (
	clientRoutines  = []string{"vcl_recv", "vcl_pipe", "vcl_pass", "vcl_hit", "vcl_miss", "vcl_hash", "vcl_purge", "vcl_deliver", "vcl_synth"}
	backendRoutines = []string{"vcl_backend_error", "vcl_backend_response", "vcl_backend_error_fetch"}

	allRoutines = x()

	variables = map[string][]string{
		"local.ip":        allRoutines,
		"local.endpoint":  allRoutines,
		"local.socket":    allRoutines,
		"remote.ip":       allRoutines,
		"client.ip":       allRoutines,
		"client.identity": clientRoutines,
		"server.ip":       allRoutines,
		"server.hostname": allRoutines,
		"server.identity": allRoutines,

		"req":                  clientRoutines,
		"req.method":           clientRoutines,
		"req.url":              clientRoutines,
		"req.hash":             {"vcl_hit", "vcl_miss", "vcl_pass", "vcl_purge", "vcl_deliver"},
		"req.http.*":           clientRoutines, // TODO: don't forget about *
		"req.proto":            clientRoutines,
		"req.restarts":         clientRoutines,
		"req.storage":          clientRoutines,
		"req.esi_level":        clientRoutines,
		"req.ttl":              clientRoutines,
		"req.grace":            clientRoutines,
		"req.xid":              clientRoutines,
		"req.esi":              clientRoutines,
		"req.can_gzip":         clientRoutines,
		"req.backend_hint":     clientRoutines,
		"req.hash_ignore_busy": clientRoutines,
		"req.hash_always_miss": clientRoutines,
		"req.is_hitmiss":       clientRoutines,
		"req.is_hitpass":       clientRoutines,
		"req_top.method":       clientRoutines,
		"req_top.url":          clientRoutines,
		"req_top.http.*":       clientRoutines,
		"req_top,proto":        clientRoutines,

		"bereq":                       backendRoutines,
		"bereq.xid":                   backendRoutines,
		"bereq.retries":               backendRoutines,
		"bereq.backend":               append(backendRoutines, "vcl_pipe"),
		"bereq.body":                  {"vcl_backend_fetch"},
		"bereq.hash":                  append(backendRoutines, "vcl_pipe"),
		"bereq.method":                append(backendRoutines, "vcl_pipe"),
		"bereq.url":                   append(backendRoutines, "vcl_pipe"),
		"bereq.proto":                 append(backendRoutines, "vcl_pipe"),
		"bereq.http.*":                append(backendRoutines, "vcl_pipe"),
		"bereq.uncacheable":           backendRoutines,
		"bereq.connect_timeout":       append(backendRoutines, "vcl_pipe"),
		"bereq.first_byte_timeout":    backendRoutines,
		"bereq.between_bytes_timeout": backendRoutines,
		"bereq.is_bgfetch":            backendRoutines,

		"beresp":              {"vcl_backend_response", "vcl_backend_error"},
		"beresp.body":         {"vcl_backend_error"},
		"beresp.proto":        {"vcl_backend_response", "vcl_backend_error"},
		"beresp.status":       {"vcl_backend_response", "vcl_backend_error"},
		"beresp.reason":       {"vcl_backend_response", "vcl_backend_error"},
		"beresp.http.*":       {"vcl_backend_response", "vcl_backend_error"},
		"beresp.do_esi":       {"vcl_backend_response", "vcl_backend_error"},
		"beresp.do_stream":    {"vcl_backend_response", "vcl_backend_error"},
		"beresp.do_gzip":      {"vcl_backend_response", "vcl_backend_error"},
		"beresp.do_gunzip":    {"vcl_backend_response", "vcl_backend_error"},
		"beresp.was_304":      {"vcl_backend_response", "vcl_backend_error"},
		"beresp.uncacheable":  {"vcl_backend_response", "vcl_backend_error"},
		"beresp.ttl":          {"vcl_backend_response", "vcl_backend_error"},
		"beresp.age":          {"vcl_backend_response", "vcl_backend_error"},
		"beresp.grace":        {"vcl_backend_response", "vcl_backend_error"},
		"beresp.keep":         {"vcl_backend_response", "vcl_backend_error"},
		"beresp.backend":      {"vcl_backend_response", "vcl_backend_error"},
		"beresp.backend.name": {"vcl_backend_response", "vcl_backend_error"},
		"beresp.backend.ip":   {"vcl_backend_response"},
		"beresp.storage":      {"vcl_backend_response", "vcl_backend_error"},
		"beresp.storage_hint": {"vcl_backend_response", "vcl_backend_error"},
		"beresp.filters":      {},

		"obj.proto":       {"vcl_hit"},
		"obj.status":      {"vcl_hit"},
		"obj.reason":      {"vcl_hit"},
		"obj.hits":        {"vcl_hit", "vcl_deliver"},
		"obj.http.*":      {"vcl_hit"},
		"obj.ttl":         {"vcl_hit", "vcl_deliver"},
		"obj.age":         {"vcl_hit", "vcl_deliver"},
		"obj.grace":       {"vcl_hit", "vcl_deliver"},
		"obj.keep":        {"vcl_hit", "vcl_deliver"},
		"obj.uncacheable": {"vcl_deliver"},
		"obj.storage":     {"vcl_hit", "vcl_deliver"},

		"resp.body":         {"vcl_synth"},
		"resp.proto":        {"vcl_deliver", "vcl_synth"},
		"resp.status":       {"vcl_deliver", "vcl_synth"},
		"resp.reason":       {"vcl_deliver", "vcl_synth"},
		"resp.http.*":       {"vcl_deliver", "vcl_synth"},
		"resp.do_esi":       {"vcl_deliver", "vcl_synth"},
		"resp.is_streaming": {"vcl_deliver", "vcl_synth"},
		"resp.filters":      {},

		"now": allRoutines,

		"sess.timeout_idle": allRoutines,
		"sess.xid":          allRoutines,
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
		token.LET:         LESSGREATER,
		token.GET:         LESSGREATER,
		token.PLUS:        SUM,
		token.MINUS:       SUM,
		token.SLASH:       PRODUCT,
		token.ASTERISK:    PRODUCT,
		token.LPAREN:      CALL,
	}
)

type (
	prefixParseFn func() (ast.Expression, error)
	infixParseFn  func(ast.Expression) (ast.Expression, error)
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
	p.registerPrefix(token.ACL, p.parseAclExpression)
	p.registerPrefix(token.BACKEND, p.parseBackendExpression)
	p.registerPrefix(token.PROBE, p.parseProbeExpression)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)
	p.registerPrefix(token.IF, p.parseIfExpression)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.FALSE, p.parseBoolean)
	p.registerPrefix(token.ATTRIBUTE, p.parseAttribute)

	p.registerInfix(token.LPAREN, p.parseCallExpression)
	p.registerInfix(token.ASSIGN, p.parseInfixExpression)
	p.registerInfix(token.MATCH, p.parseInfixExpression)
	p.registerInfix(token.NOT_MATCH, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GET, p.parseInfixExpression)
	p.registerInfix(token.LET, p.parseInfixExpression)
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

func (p *Parser) ParseProgram() (*ast.Program, error) {
	var err error
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.currentToken.Type != token.EOF {
		stmt, e := p.parseStatement()

		if e != nil {
			errMsg := fmt.Sprintf("Parse error line %d: %s", p.l.CurrentLine(), e.Error())
			return program, errors.New(errMsg)
		}

		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}

		p.nextToken()
	}

	return program, err
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

func (p *Parser) parseBoolean() (ast.Expression, error) {
	return &ast.Boolean{Token: p.currentToken, Value: p.currentTokenIs(token.TRUE)}, nil
}

func (p *Parser) parsePrefixExpression() (ast.Expression, error) {
	expression := &ast.PrefixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
	}

	p.nextToken()

	right, err := p.parseExpression(PREFIX)
	expression.Right = right

	return expression, err
}

func (p *Parser) parseInfixExpression(left ast.Expression) (ast.Expression, error) {
	expression := &ast.InfixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
		Left:     left,
	}

	precedence := p.currentPrecedence()

	p.nextToken()

	right, err := p.parseExpression(precedence)
	expression.Right = right

	return expression, err
}

func (p *Parser) parseStatement() (ast.Statement, error) {
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
}

func (p *Parser) parseExpressionStatement() (*ast.ExpressionStatement, error) {
	stmt := &ast.ExpressionStatement{Token: p.currentToken}

	exp, err := p.parseExpression(LOWEST)

	if err != nil {
		return stmt, err
	}

	stmt.Expression = exp

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt, nil
}

func (p *Parser) parseExpression(precedence int) (ast.Expression, error) {
	var err error
	prefix := p.prefixParseFns[p.currentToken.Type]

	if prefix == nil {
		return nil, errors.New("Did not find parsing function for " + p.currentToken.Literal)
	}

	leftExp, err := prefix()

	if err != nil {
		return nil, err
	}

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {

		infix := p.infixParseFns[p.peekToken.Type]

		if infix == nil {
			return leftExp, errors.New("Did not find infix parsing function for " + p.currentToken.Literal)
		}

		p.nextToken()

		leftExp, err = infix(leftExp)

		if err != nil {
			break
		}
	}

	return leftExp, err
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

func (p *Parser) parseReturnStatement() (*ast.ReturnStatement, error) {
	stmt := &ast.ReturnStatement{Token: p.currentToken}

	p.nextToken()

	rv, err := p.parseGroupedExpression()

	if err != nil {
		return stmt, err
	}

	stmt.ReturnValue = rv

	for !p.currentTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt, nil
}

func (p *Parser) parseCallArguments() ([]ast.Expression, error) {
	args := []ast.Expression{}

	var err error

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return args, err
	}

	p.nextToken()

	arg, err := p.parseExpression(LOWEST)

	if err != nil {
		return args, err
	}

	args = append(args, arg)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()

		arg, err := p.parseExpression(LOWEST)

		if err != nil {
			return args, nil
		}
		args = append(args, arg)
	}

	if !p.expectPeek(token.RPAREN) {
		return args, errors.New("Unclosed right paranthesis when parsing function arguments.")

	}

	return args, nil
}

func (p *Parser) parseCallExpression(function ast.Expression) (ast.Expression, error) {
	exp := &ast.CallExpression{Token: p.currentToken, Function: function}
	args, err := p.parseCallArguments()

	if err != nil {
		return exp, err
	}

	exp.Arguments = args

	return exp, err
}

func (p *Parser) parseGroupedExpression() (ast.Expression, error) {
	p.nextToken()

	exp, err := p.parseExpression(LOWEST)

	if err != nil {
		return exp, err
	}

	if !p.expectPeek(token.RPAREN) {
		return exp, errors.New("Fill this in") //TODO:
	}

	return exp, err
}

func (p *Parser) currentTokenIs(t token.TokenType) bool {
	return p.currentToken.Type == t
}

func (p *Parser) currentTokenIn(tks []token.TokenType) bool {

	for _, tk := range tks {

		if p.currentToken.Type == tk {
			return true
		}
	}

	return false
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
	errMsg := fmt.Sprintf("Line %d: Expected next token to be %s but got %s %s instead.", p.l.CurrentLine(), t, p.peekToken.Type, p.peekToken.Literal)
	p.errors = append(p.errors, errMsg)
}

func (p *Parser) nextToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) parseIntegerLiteral() (ast.Expression, error) {
	lit := &ast.IntegerLiteral{Token: p.currentToken}

	value, err := strconv.ParseInt(p.currentToken.Literal, 0, 64)

	if err != nil {
		return lit, err
	}

	lit.Value = value

	return lit, err
}

func (p *Parser) parseRealLiteral() (ast.Expression, error) {
	rl := &ast.RealLiteral{Token: p.currentToken}

	value, err := strconv.ParseFloat(p.currentToken.Literal, 64)

	if err != nil {
		return rl, err
	}

	rl.Value = value

	return rl, err
}

func (p *Parser) parseElseIfExpression() (ast.Expression, error) {
	ei := &ast.ElseIfExpression{Token: p.currentToken}

	p.nextToken()

	if !p.currentTokenIs(token.LPAREN) {
		return nil, errors.New("Was expecting an open ( when parsing elseIf condition")
	}

	p.nextToken()

	condition, err := p.parseExpression(LOWEST)

	if err != nil {
		return ei, err
	}

	ei.Condition = condition

	if !p.expectPeek(token.RPAREN) {
		return nil, errors.New("Was expecting an closing ) when parsing elseIf condition")
	}

	if !p.expectPeek(token.LBRACE) {
		return nil, errors.New("Was expecting an open { when parsing elseIf block")
	}

	consequence, err := p.parseBlockStatement()

	if err != nil {
		return ei, err
	}

	ei.Consequence = consequence

	return ei, nil
}

func (p *Parser) parseIfExpression() (ast.Expression, error) {
	expression := &ast.IfExpression{Token: p.currentToken}
	expression.Alternatives = make([]ast.Expression, 0)

	var err error

	// TODO: handle all the err returns, create actual errors

	if !p.expectPeek(token.LPAREN) {
		return expression, err
	}

	p.nextToken()

	condition, err := p.parseExpression(LOWEST)

	if err != nil {
		return expression, err
	}

	expression.Condition = condition

	if !p.expectPeek(token.RPAREN) {
		return expression, err
	}

	if !p.expectPeek(token.LBRACE) {
		return expression, err
	}

	conseq, err := p.parseBlockStatement()

	if err != nil {
		return expression, err
	}

	expression.Consequence = conseq

	for p.peekTokenIs(token.ELSEIF) {
		p.nextToken()

		bs, err := p.parseElseIfExpression()

		if err != nil {
			return expression, err
		}

		expression.Alternatives = append(expression.Alternatives, bs)
	}

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()

		if !p.expectPeek(token.LBRACE) {
			return expression, err
		}

		alt, err := p.parseBlockStatement()

		if err != nil {
			return expression, err
		}

		expression.Alternative = alt
	}

	return expression, err
}

func (p *Parser) parseDurationLiteral() (ast.Expression, error) {
	dl := &ast.DurationLiteral{Token: p.currentToken, Value: p.currentToken.Literal}

	return dl, nil
}

func (p *Parser) parseStringLiteral() (ast.Expression, error) {
	stl := &ast.StringLiteral{Token: p.currentToken, Value: p.currentToken.Literal}
	return stl, nil
}

func (p *Parser) parseAttribute() (ast.Expression, error) {
	at := &ast.Attribute{Token: p.currentToken}
	at.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.expectPeek(token.ASSIGN) {
		return at, errors.New("Missing assignment in attribute " + p.currentToken.Literal)
	}

	p.nextToken()

	// this if test is quite a hack.
	// it's the use case when you define
	// a probe within a backend like this:
	// .probe = { ... }
	if at.TokenLiteral() == ".probe" && p.currentToken.Type == token.LBRACE {
		allwd := []token.TokenType{token.ATTRIBUTE}

		_, err := p.parseRestrictedBlockStatement(allwd, token.PROBE)

		if err != nil {
			return nil, err
		}

	} else {

		v, err := p.parseExpression(LOWEST)

		if err != nil {
			return at, err
		}

		at.Value = v

		if !p.expectPeek(token.SEMICOLON) {
			p.nextToken()
		}
	}

	return at, nil
}

func (p *Parser) parseSetStatement() (*ast.SetStatement, error) {
	stmt := &ast.SetStatement{Token: p.currentToken}

	if !p.expectPeek(token.IDENT) {
		return stmt, errors.New("Missing identifier in set statement")
	}

	err := p.isValidVariable(p.currentToken.Literal)

	if err != nil {
		return stmt, err
	}

	stmt.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.expectPeek(token.ASSIGN) {
		return stmt, errors.New("Missing assignment in set statement")
	}

	p.nextToken()

	v, err := p.parseExpression(LOWEST)

	if err != nil {
		return stmt, err
	}

	stmt.Value = v

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt, nil
}

func (p *Parser) parseVclStatement() (*ast.VclStatement, error) {
	stmt := &ast.VclStatement{Token: p.currentToken}

	if !p.expectPeek(token.REAL) {
		return stmt, errors.New("Was expecting a real value when parsing vcl version")
	}

	v, err := strconv.ParseFloat(p.currentToken.Literal, 64)

	if err != nil {
		p.errors = append(p.errors, err.Error())
		return stmt, err
	}

	stmt.Value = v

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt, nil

}

func (p *Parser) parseIncludeStatement() (*ast.IncludeStatement, error) {
	stmt := &ast.IncludeStatement{Token: p.currentToken}

	p.nextToken()

	stmt.Value = p.currentToken.Literal

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt, nil
}

func (p *Parser) parseCallFunctionStatement() (*ast.CallStatement, error) {
	stmt := &ast.CallStatement{Token: p.currentToken}

	p.nextToken()

	stmt.Value = p.currentToken.Literal

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt, nil
}

func (p *Parser) parseUnsetStatement() (*ast.UnsetStatement, error) {
	stmt := &ast.UnsetStatement{Token: p.currentToken}

	p.nextToken()

	stmt.Value = p.currentToken.Literal

	err := p.isValidVariable(stmt.Value)
	if err != nil {
		return stmt, err
	}

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt, err
}

func (p *Parser) parseImportStatement() (*ast.ImportStatement, error) {
	stmt := &ast.ImportStatement{Token: p.currentToken}

	p.nextToken()

	stmt.Value = p.currentToken.Literal

	if !p.expectPeek(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt, nil
}

func (p *Parser) isValidVariable(vclVar string) error {

	if sub, found := variables[vclVar]; found {
		for _, s := range sub {
			if s == p.currentSub {
				return nil
			}
		}

		return errors.New(fmt.Sprintf("%s is not allowed in %s", vclVar, p.currentSub))
	}

	return nil
}

func (p *Parser) parseIdentifier() (ast.Expression, error) {

	err := p.isValidVariable(p.currentToken.Literal)

	return &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}, err
}

func (p *Parser) parseBlockStatement() (*ast.BlockStatement, error) {
	block := &ast.BlockStatement{Token: p.currentToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
		stmt, err := p.parseStatement()

		if err != nil {
			return block, err
		}

		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}

		p.nextToken()
	}

	if p.currentToken.Type != token.RBRACE {
		return block, errors.New("Unclosed curly brace.")
	}

	return block, nil
}

func (p *Parser) parseRestrictedBlockStatement(tks []token.TokenType, parent token.TokenType) (*ast.BlockStatement, error) {
	block := &ast.BlockStatement{Token: p.currentToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {

		if !p.currentTokenIn(tks) {
			return block, errors.New(fmt.Sprintf("%s token not allowed in %s", p.currentToken.Type, parent))
		}

		stmt, err := p.parseStatement()

		if err != nil {
			return block, err
		}

		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}

		p.nextToken()
	}

	if p.currentToken.Type != token.RBRACE {
		return block, errors.New("Unclosed curly brace.")
	}

	return block, nil
}

func (p *Parser) parseAclExpression() (ast.Expression, error) {
	acl := &ast.AclExpression{Token: p.currentToken}

	p.nextToken()

	acl.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	p.nextToken()

	allwd := []token.TokenType{token.STRING, token.BANG}

	body, err := p.parseRestrictedBlockStatement(allwd, acl.Token.Type)

	if err != nil {
		return acl, err
	}

	acl.Body = body

	//TODO: we can get into detailed parsing here. ipv4, ipv6...etc

	return acl, nil
}

func (p *Parser) parseBackendExpression() (ast.Expression, error) {
	backend := &ast.BackendExpression{Token: p.currentToken}

	p.nextToken()

	backend.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	p.nextToken()

	allwd := []token.TokenType{token.ATTRIBUTE}

	body, err := p.parseRestrictedBlockStatement(allwd, backend.Token.Type)

	if err != nil {
		return backend, err
	}

	backend.Body = body

	return backend, nil
}

func (p *Parser) parseProbeExpression() (ast.Expression, error) {
	probe := &ast.ProbeExpression{Token: p.currentToken}

	p.nextToken()

	probe.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	p.nextToken()

	allwd := []token.TokenType{token.ATTRIBUTE}

	body, err := p.parseRestrictedBlockStatement(allwd, probe.Token.Type)

	if err != nil {
		return probe, err
	}

	probe.Body = body

	return probe, nil
}

func (p *Parser) parseFunctionLiteral() (ast.Expression, error) {
	fn := &ast.FunctionLiteral{Token: p.currentToken}

	p.nextToken()

	fn.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	//if strings.HasPrefix(fn.Name.Value, "vcl_") {
	p.currentSub = fn.Name.Value
	//}

	p.nextToken()

	body, err := p.parseBlockStatement()

	if err != nil {
		return fn, err
	}

	fn.Body = body

	return fn, nil
}
