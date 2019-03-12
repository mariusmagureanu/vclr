package parser

import (
	_ "fmt"
	"testing"

	"github.com/varnish/vclr/ast"
	"github.com/varnish/vclr/lexer"
)

func TestAll(t *testing.T) {
	input := `
		vcl 4.1;
		
		import std;
		import curl;
		
		include "foo.vcl";
		include "bar.vcl";
		
		acl localnetwork {
    		"localhost";   
    		"192.0.2.0"/24; 
		     !"192.0.2.23"; 
		}

		probe demo {
			.url = "/api/v1/foo";
			.expected_response = 200;
			.interval = 5s;
			.window = 8;
			}
			
		backend first {
		.host = "localhost";
		.port = 7085;
		.connect_timeout=50m;
		}
	
		backend second {
		.host = "localhost";
		.port = 9001;
		.connect_timeout=10s;
		}
		
		sub vcl_recv {
			
			if (req.method == "PURGE") {
    			if (client.ip ~ local) {
    			   return(purge);
    			} else {
    			    return (synth(403,("blaa")));
   				 }
  			}
	
			set req.http.X-foo = "bar";
			set req.url = "/api/url/baz";
			
			  if (req.method != "GET" &&
      			  req.method != "HEAD" &&
      			  req.method != "PUT" &&
      			  req.method != "POST" &&
      			  req.method != "TRACE" &&
      			  req.method != "OPTIONS" &&
      			  req.method != "DELETE") 
					{
        	   			return (pipe);
				}
		}
		
		sub vcl_pass {
			set req.http.X-Baz = "qux";
			
			set req.http.X-Forwarded-For = req.http.X-Forwarded-For + ", " + client.ip;
			
			if (client.ip ~ localnetwork) {
    			unset req.http.cookie;
				set req.url = "/api/v1/foo?param=%#baz";
			} else {
				set req.http.cookie = "Cache-Control: blaa";
				}
		}
	
	`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	checkParseErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 11 {
		t.Fatalf("program.Statements does not contain %d statements. got %d", 11, len(program.Statements))
	}

	//fmt.Println(program.String())
}

func TestFunctionLiteral(t *testing.T) {
	input := `
	sub vcl_recv {
		set req.http.X-foo = "bar";
		unset req.http.cookie;
		set req.http.X-Forwarded-For = req.http.X-Forwarded-ForZ + ", " + client.ip;
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

	fnExp, ok := exp.Expression.(*ast.FunctionLiteral)

	if !ok {
		t.Fatal("fnExp is not a Function literal.")
	}

	if len(fnExp.Body.Statements) != 3 {
		t.Fatalf("Was expecting %d statement in the function, got %d instead.", 3, len(fnExp.Body.Statements))
	}

	setStmt, ok := fnExp.Body.Statements[0].(*ast.SetStatement)

	if !ok {
		t.Fatal("The statement in the function is not a set statement")
	}

	if setStmt.Name.Value != "req.http.X-foo" {
		t.Fatal("Set statement has the incorrect identifier name.")
	}

	if setStmt.Value.TokenLiteral() != "bar" {
		t.Fatal("Set statement value has the incorrect value.")
	}

}

func TestProbeExpression(t *testing.T) {
	input := `
		probe demo {
			.url = "/api/v1/foo";
			.expected_response = 200;
			.interval = 5s;
			.window = 8;
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

	probeExp, ok := exp.Expression.(*ast.ProbeExpression)

	if !ok {
		t.Fatal("probeExp is not a probe expression")
	}

	if len(probeExp.Body.Statements) != 4 {
		t.Fatalf("Was expecting %d probe attributes, got %d instead.", 4, len(probeExp.Body.Statements))
	}

}

func TestBackendExpression(t *testing.T) {
	input := `
	backend default {
		.host = "localhost";
		.port = 9001;
		.connect_timeout=10s;
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

	backendExp, ok := exp.Expression.(*ast.BackendExpression)

	if !ok {
		t.Fatal("backendExp is not a backend expression.")
	}

	if len(backendExp.Body.Statements) != 3 {
		t.Fatalf("Backend definition was supposed to have %d attributes, got %d instead", 3, len(backendExp.Body.Statements))
	}

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
set x = 5+3;
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
		expectedValue      string
	}{
		{"x", "5+3"},
		{"y", "10"},
		{"foobar", "838383"},
	}

	for i, tt := range tests {
		stmt := program.Statements[i]
		if !testSetStatement(t, stmt, tt.expectedIdentifier, tt.expectedValue) {
			t.Fail()
		}
	}
}

func testSetStatement(t *testing.T, s ast.Statement, name string, value string) bool {
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

	if letStmt.Value.String() != value {
		t.Errorf("letStmt.Value.String is not %s, got %s", value, letStmt.Value.String())
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
