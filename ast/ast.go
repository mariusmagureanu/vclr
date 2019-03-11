package ast

import (
	"bytes"
	"strconv"

	"github.com/varnish/vclr/token"
)

type Node interface {
	TokenLiteral() string
	String() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
}

type PrefixExpression struct {
	Token    token.Token
	Operator string
	Right    Expression
}

type InfixExpression struct {
	Token    token.Token
	Left     Expression
	Operator string
	Right    Expression
}

type SetStatement struct {
	Token token.Token
	Name  *Identifier
	Value Expression
}

type VclStatement struct {
	Token token.Token
	Name  *Identifier
	Value float64
}

type IncludeStatement struct {
	Token token.Token
	Value string
}

type ImportStatement struct {
	Token token.Token
	Value string
}

type ReturnStatement struct {
	Token       token.Token
	ReturnValue Expression
}

type Identifier struct {
	Token token.Token
	Value string
}

type Boolean struct {
	Token token.Token
	Value bool
}

type IntegerLiteral struct {
	Token token.Token
	Value int64
}

type RealLiteral struct {
	Token token.Token
	Value float64
}

type StringLiteral struct {
	Token token.Token
	Value string
}

type BlockStatement struct {
	Token      token.Token // the { token
	Statements []Statement
}

type AclExpression struct {
	Token token.Token // the acl token
	Name  *Identifier
	Body  *BlockStatement
}

type BackendExpression struct {
	Token token.Token
	Name  *Identifier
	Body  *BlockStatement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

func (p *Program) String() string {
	var out bytes.Buffer

	for _, s := range p.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}

func (ls *SetStatement) statementNode()       {}
func (ls *SetStatement) TokenLiteral() string { return ls.Token.Literal }
func (ls *SetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	out.WriteString(";")

	return out.String()
}

func (pe *PrefixExpression) expressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("{")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString("}")

	return out.String()

}

func (ie *InfixExpression) expressionNode()      {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("{")
	out.WriteString(ie.Left.String())
	out.WriteString(ie.Operator)
	out.WriteString(ie.Right.String())
	out.WriteString("}")

	return out.String()
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string {
	return i.Value
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")

	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}

	out.WriteString(";")

	return out.String()
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}

	return ""
}

func (vs *VclStatement) statementNode()       {}
func (vs *VclStatement) TokenLiteral() string { return vs.Token.Literal }
func (vs *VclStatement) String() string {
	return vs.TokenLiteral() + " " + strconv.FormatFloat(vs.Value, 'f', 1, 64)
}

func (vs *ImportStatement) statementNode()       {}
func (vs *ImportStatement) TokenLiteral() string { return vs.Token.Literal }
func (vs *ImportStatement) String() string {
	return vs.TokenLiteral() + " " + vs.Value
}

func (vs *IncludeStatement) statementNode()       {}
func (vs *IncludeStatement) TokenLiteral() string { return vs.Token.Literal }
func (vs *IncludeStatement) String() string {
	return vs.TokenLiteral() + " " + vs.Value
}

func (bs *BlockStatement) expressionNode()      {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out bytes.Buffer

	for _, s := range bs.Statements {
		out.WriteString("\n")
		out.WriteString(s.String())

	}

	return out.String()
}

func (ac *AclExpression) expressionNode()      {}
func (ac *AclExpression) TokenLiteral() string { return ac.Token.Literal }
func (ac *AclExpression) String() string {
	var out bytes.Buffer

	out.WriteString(ac.Token.Literal)
	out.WriteString(" ")
	out.WriteString(ac.Name.Value)
	out.WriteString(ac.Body.String())

	return out.String()
}

func (be *BackendExpression) expressionNode()      {}
func (be *BackendExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BackendExpression) String() string {
	var out bytes.Buffer

	out.WriteString(be.Token.Literal)
	out.WriteString(" ")
	out.WriteString(be.Name.Value)
	out.WriteString(be.Body.String())

	return out.String()
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

func (rl *RealLiteral) expressionNode()      {}
func (rl *RealLiteral) TokenLiteral() string { return rl.Token.Literal }
func (rl *RealLiteral) String() string       { return strconv.FormatFloat(rl.Value, 'f', 1, 64) }

func (ip *StringLiteral) expressionNode()      {}
func (ip *StringLiteral) TokenLiteral() string { return ip.Token.Literal }
func (ip *StringLiteral) String() string       { return ip.Value }
