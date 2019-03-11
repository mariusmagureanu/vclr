package parser

import (
	"fmt"
	"testing"

	"github.com/varnish/vclr/ast"
	"github.com/varnish/vclr/lexer"
)

func TestBackendExpression(t *testing.T) {
	input := `
	backend default {
		.host = "localhost";
		.port = 9001;
		.connect_timeout=10s;
`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	checkParseErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statements. got %d", len(program.Statements))
	}

	exp, ok := program.Statements[0].(*ast.ExpressionStatement)

	if !ok {
		t.Fatal("Expression is not expression statement.")
	}

	fmt.Println(exp.String())
}

func TestAclStatement(t *testing.T) {
	input := `
	acl localnetwork {
    "localhost";   
    "192.0.2.0"/24; 
     !"192.0.2.23"; 
}
	`
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	checkParseErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statements. got %d", len(program.Statements))
	}

	exp, ok := program.Statements[0].(*ast.ExpressionStatement)

	if !ok {
		t.Fatal("Expression is not expression statement.")
	}

	aclExp, ok := exp.Expression.(*ast.AclExpression)

	if !ok {
		t.Fatal("aclExp is not an acl expression")
	}

	if len(aclExp.Body.Statements) != 3 {
		t.Fatal("Should have been 3 ips in the acl.")
	}

}

func TestImportStatement(t *testing.T) {
	input := `import std;`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	checkParseErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statements. got %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ImportStatement)

	if !ok {
		t.Fatal("Statement is not an import statement.")
	}

	importedValue := stmt.Value

	if importedValue != "std" {
		t.Fatalf("was expecting std, got %s", importedValue)
	}

}

func TestIncludeStatement(t *testing.T) {
	input := `include "foo.vcl";
	`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	checkParseErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statements. got %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.IncludeStatement)

	if !ok {
		t.Fatal("Statement is not an include statement.")
	}

	includedValue := stmt.Value

	if includedValue != "foo.vcl" {
		t.Fatalf("was expecting foo.vcl, got %s", includedValue)
	}

}

func TestVclStatement(t *testing.T) {
	input := `vcl 4.1;
	
	vcl 4.2;`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	checkParseErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 2 {
		t.Fatalf("program.Statements does not contain 2 statements. got %d", len(program.Statements))
	}

}

func TestSetStatements(t *testing.T) {
	input := `
set x = 5;
set y = 10;
set foobar = 838383;
`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	checkParseErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 3 {
		t.Fatalf("program.Statements does not contain 3 statements. got %d", len(program.Statements))
	}

	tests := []struct {
		expectedIdentifier string
	}{
		{"x"},
		{"y"},
		{"foobar"},
	}

	for i, tt := range tests {
		stmt := program.Statements[i]
		if !testSetStatement(t, stmt, tt.expectedIdentifier) {
			return
		}
	}
}

func testSetStatement(t *testing.T, s ast.Statement, name string) bool {
	if s.TokenLiteral() != "set" {
		t.Errorf("s.TokenLitelral is not 'let', got %q", s.TokenLiteral())
		return false
	}

	letStmt, ok := s.(*ast.SetStatement)

	if !ok {
		t.Errorf("s is not a *ast.Statement, got %T", s)
		return false
	}

	if letStmt.Name.Value != name {
		t.Errorf("letStmt.Name.Value not '%s', got %s", name, letStmt.Name.Value)
		return false
	}

	if letStmt.Name.TokenLiteral() != name {
		t.Errorf("letStmt.Name.TokenLiteral() not %s, got %s", name, letStmt.Name.TokenLiteral())
		return false
	}

	return true
}

func checkParseErrors(t *testing.T, p *Parser) {
	errors := p.Errors()

	if len(errors) == 0 {
		return
	}

	t.Errorf("Parser has %d errors", len(errors))

	for _, msg := range errors {
		t.Errorf("parser error: %s", msg)
	}

	t.FailNow()
}
