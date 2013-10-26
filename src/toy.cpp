#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <cerrno>
#include <algorithm>
#include "llvm/Analysis/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

using namespace std;

vector<string> split(const string &str, char delim){
    istringstream iss(str); string tmp; vector<string> res;
    while(getline(iss, tmp, delim)) res.push_back(tmp);
    return res;
}

class Token {
    public:
	static const string DEF;
	static const string EXTERN;
	static const string LBRANCKET;
	static const string RBRANCKET;
	static const string COMMENT;
	static const string END;
	static const string PLUS;
	static const string MINUS;
	static const string MULTIPLY;
	static const string LESS_THAN;

	static const string EOL;

	enum TokenType {
	    TOKEN_EOF = 0, TOKEN_DEF, TOKEN_EXTERN, TOKEN_IDENTIFIER, TOKEN_NUMBER, TOKEN_LBRANCKET, TOKEN_RBRANCKET, TOKEN_BINOP, TOKEN_EOL, TOKEN_OTHERS
	};

	Token () {}

	Token(string token) {
	    this->_token = token;
	    this->_type = TOKEN_OTHERS;

	    if (token == Token::DEF) this->_type = TOKEN_DEF;
	    else if (token == Token::EXTERN) this->_type = TOKEN_EXTERN;
	    else if (token == Token::LBRANCKET) this->_type = TOKEN_LBRANCKET;
	    else if (token == Token::RBRANCKET) this->_type = TOKEN_RBRANCKET;
	    else if (token == Token::END) this->_type = TOKEN_EOF;
	    else if (token == Token::EOL) this->_type = TOKEN_EOL;
	    else if (token == PLUS || token == MINUS || token == MULTIPLY || token == LESS_THAN) {
		this->_binop = token.c_str()[0];
		this->_type = TOKEN_BINOP;
	    }
	    else {
		this->_type = TOKEN_IDENTIFIER;
		for (string::iterator it = token.begin(), end = token.end(); it != end; it++) {
		    if (!isalnum(*it)) this->_type = TOKEN_OTHERS;
		}
	    }

	    char* err = NULL;
	    this->_tokenVal = strtod((char *)(token.c_str()), &err);
	    if (strlen(err) == 0) this->_type = TOKEN_NUMBER;
	}

	string getToken() {return _token;}
	char getBinOp() {return _binop;}
	int getType() {return _type;}
	double getTokenVal() {return _tokenVal;}

    private:
	int _type;
	string _token;
	char _binop;
	double _tokenVal;
};

const string Token::DEF     = "def";
const string Token::EXTERN  = "extern";
const string Token::LBRANCKET = "(";
const string Token::RBRANCKET = ")";
const string Token::COMMENT = "#";
const string Token::END = "@@END@@";
const string Token::PLUS = "+";
const string Token::MINUS = "-";
const string Token::MULTIPLY = "*";
const string Token::LESS_THAN = "<";
const string Token::EOL= ";";


class Lexer {
    public:
	Lexer() {}
	Lexer (string input) 
	    : _tokenPtr(0)
	{
	    this->mkTokens(input);
	}

	void mkTokens(string input) {
	    string token;
	    for (string::iterator it = input.begin(), end = input.end(); it != end;) {
		while (isspace(*it)) it++;

		if (isalpha(*it)) {
		    token = (*it);
		    while (isalnum(*(++it))) token += (*it);
		    this->_tokens.push_back(token);
		    continue;
		}

		if (isdigit(*it) || (*it) == '.') {
		    token = (*it);
		    while (isdigit(*(++it)) || (*it) == '.') token += (*it);
		    this->_tokens.push_back(token);
		    continue;
		}

		if ((*it) == '#') {
		    while ((*it) != EOF && (*it) != '\n' && (*it) != '\r');
		    continue;
		}

		token = *(it++);
		this->_tokens.push_back(token);
	    }
	}

	Token getToken() {
	    string token = this->getNextToken();

	    if (token == Token::COMMENT) {
		do { token = this->getNextToken();  } while(token != "\\n" && token != "\\r" && token != Token::END);
	    }

	    return Token(token);
	}
    private:
	string getNextToken() {
	    if (this->_tokenPtr >= this->_tokens.size()) return Token::END;
	    return this->_tokens[this->_tokenPtr++];
	}

	vector<string> _tokens;
	int _tokenPtr;
};


class ExprAST {
    public:
	virtual ~ExprAST() {}
};

class NumberExprAST : public ExprAST {
    public:
	NumberExprAST(double val)
	    : _val(val) {}
    private:
	double _val;
};

class VariantExprAST : public ExprAST {
    public:
	VariantExprAST(string name) 
	    : _name(name) {}
    private:
	string _name;
};

class BinaryExprAST : public ExprAST {
    public:
	BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs)
	    : _op(op), _lhs(lhs), _rhs(rhs) {}
    private:
	char _op;
	ExprAST *_lhs, *_rhs;
};

class CallExprAST : public ExprAST {
    public:
	CallExprAST(const string &callee, vector<ExprAST*> &args)
	    : _callee(callee), _args(args) {}

    private:
	string _callee;
	vector<ExprAST*> _args;
};

class PrototypeAST {
    public:
	PrototypeAST(const string &name, const vector<std::string> &args)
	    : _name(name), _args(args) {}
    private:
	string _name;
	vector<std::string> _args;
};

class FunctionAST {
    public:
	FunctionAST(PrototypeAST *proto, ExprAST *body)
	    : _proto(proto), _body(body) {}
    private:
	PrototypeAST *_proto;
	ExprAST *_body;
};

class Parser {
    public:
	Parser() 
	{
	    this->_precedence['<'] = 10;
	    this->_precedence['+'] = 20;
	    this->_precedence['-'] = 30;
	    this->_precedence['*'] = 40;
	}

	ExprAST* parserNumber() {
	    ExprAST *result = new NumberExprAST(_curToken.getTokenVal());
	    getNextToken(); 
	    return result;
	}

	ExprAST* parseParen() {
	    getNextToken();
	    ExprAST *v = parseExpression();
	    if (!v) return NULL;

	    if (_curToken.getToken() != Token::RBRANCKET) return printError("expected ')'");
	    getNextToken(); 
	    return v;
	}

	ExprAST* parseIdentifier() {
	    string idName = _curToken.getToken();

	    getNextToken();

	    if (_curToken.getToken() != Token::LBRANCKET) return new VariantExprAST(idName);

	    getNextToken();
	    vector<ExprAST*> args;
	    if (_curToken.getToken() != Token::RBRANCKET) {
		while (true) {
		    ExprAST* arg = parseExpression();
		    if (!arg) return NULL;
		    args.push_back(arg);

		    if (_curToken.getToken() == Token::RBRANCKET) break;

		    if (_curToken.getToken() != ",") printError("Expected ')' or ',' in argument list");
		    getNextToken();
		}
	    }

	    getNextToken();

	    return new CallExprAST(idName, args);
	}

	ExprAST* parsePrimary() {
	    switch (this->_curToken.getType()) {
		case Token::TOKEN_IDENTIFIER: return parseIdentifier();
	        case Token::TOKEN_NUMBER: return parserNumber();
		case Token::TOKEN_LBRANCKET: return parseParen();
		default: break;
	    }
	    return printError("unknown token when expecting an expression");
	}

	ExprAST* parseExpression() {
	    ExprAST* lhs = parsePrimary();
	    if (!lhs) return NULL;

	    return parseBinOpRHS(0, lhs);
	}

	ExprAST* parseBinOpRHS(int exprPrec, ExprAST* lhs) {
	    while (1) {
		int prec = getTokPrecedence();

		if (prec < exprPrec) return lhs;
		int binOp = _curToken.getBinOp();

		getNextToken();
		ExprAST* rhs = parsePrimary();

		if (!rhs) return 0;

		int nextPrec = getTokPrecedence();
		if (prec < nextPrec) {
		    rhs = parseBinOpRHS(prec+1, rhs);
		    if (rhs == 0) return 0;
		}
		lhs = new BinaryExprAST(binOp, lhs, rhs);
	    }
	}

	PrototypeAST* parsePrototype() {
	    if (this->_curToken.getType() != Token::TOKEN_IDENTIFIER)
		return printErrorProto("Expected function name in prototype");

	    string fnName = this->_curToken.getToken();
	    getNextToken();

	    if (this->_curToken.getToken() != Token::LBRANCKET)
		return printErrorProto("Expected '(' in prototype");

	    vector<string> argNames;
	    while (getNextToken().getType() == Token::TOKEN_IDENTIFIER)
		argNames.push_back(this->_curToken.getToken());
	    if (this->_curToken.getToken() != Token::RBRANCKET)
		return printErrorProto("Expected ')' in prototype");

	    getNextToken();

	    return new PrototypeAST(fnName, argNames);
	}

	FunctionAST* parseDefinition() {
	    getNextToken();
	    PrototypeAST* proto = parsePrototype();
	    if (!proto) return NULL;

	    if (ExprAST* e = parseExpression())
		return new FunctionAST(proto, e);

	    return NULL;
	}

	PrototypeAST* parseExtern() {
	    getNextToken();
	    return parsePrototype();
	}

	FunctionAST* parseTopLevelExpr() {
	    if (ExprAST *e = parseExpression()) {
		PrototypeAST *proto = new PrototypeAST("", vector<string>());
		return new FunctionAST(proto, e);
	    }
	    return 0;
	}

	void handleDefinition() {
	    if (parseDefinition()) {
		cout << "Parsed a function definition." << endl;
	    } else {
		getNextToken();
	    }
	}

	void handleExtern() {
	    if (parseExtern()) {
		cout << "Parsed an extern." << endl;
	    } else {
		getNextToken();
	    }
	}

	void handleTopLevelExpression() {
	    if (parseTopLevelExpr()) {
		cout << "Parsed a top-level expr." << endl;
	    } else {
		getNextToken();
	    }
	} 

	void parse(string input) {
	    this->_lexer = Lexer(input);
	    getNextToken();

	    switch(this->_curToken.getType()) {
		case Token::TOKEN_EOF: return;
		case Token::TOKEN_EOL: getNextToken(); break;
		case Token::TOKEN_DEF: handleDefinition(); break;
		case Token::TOKEN_EXTERN: handleExtern(); break;
		default: handleTopLevelExpression(); break;
	    }
	}
    private:
	Token getNextToken() { return (this->_curToken = this->_lexer.getToken()); }

	int getTokPrecedence() {
	    char op = this->_curToken.getBinOp();
	    if (!isascii(op)) return -1;

	    int prec = this->_precedence[op];
	    if (prec <= 0) return -1;
	    return prec;
	}

	ExprAST* printError(const string& str) { cout << str << endl; return NULL;}
	PrototypeAST* printErrorProto(const string& str) { printError(str); return NULL; }
	FunctionAST* printErrorFunc(const string& str) { printError(str); return NULL; }

	Lexer _lexer;
	Token _curToken;
	map<char, int> _precedence;
};

int main() {
    Parser parser = Parser();

    string input;
    while (true) {
	cout << "kaleidoscope> ";
	cin >> input;
	//input = "def foo(x y) x+foo(y, 4.0);";
	if (input == "exit") break;
	parser.parse(input);
    }
}
