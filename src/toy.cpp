#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <cerrno>

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
	static const string COMMENT;
	static const string END;

	enum TokenType {
	    TOKEN_EOF = 0, TOKEN_DEF, TOKEN_EXTERN, TOKEN_IDENTIFIER, TOKEN_NUMBER, TOKEN_OTHERS
	};

	Token(string token) {
	    this->_token = token;
	    this->_type = TOKEN_OTHERS;

	    if (token == Token::DEF) this->_type = TOKEN_DEF;
	    else if (token == Token::EXTERN) this->_type = TOKEN_EXTERN;
	    else if (token == Token::END) this->_type = TOKEN_EOF;

	    char* err = NULL;
	    this->_tokenVal = strtod((char *)(token.c_str()), &err);
	    if (strlen(err) == 0) this->_type = TOKEN_NUMBER;
	}

	string getToken() {return _token;}
	int getType() {return _type;}
	double getTokenVal() {return _tokenVal;}

    private:
	int _type;
	string _token;
	double _tokenVal;
};

const string Token::DEF     = "def";
const string Token::EXTERN  = "extern";
const string Token::COMMENT = "#";
const string Token::END = "@@END@@";


class Lexer {
    public:
	Lexer () {
	}

	Token getToken() {
	    string token = this->getNextToken();

	    if (token == Token::COMMENT) {
		do { token = this->getNextToken();  } while(token != "\\n" && token != "\\r" && token != Token::END);
	    }

	    return Token(token);
	}

	bool parse(string input) {
	    this->_tokens = split(input, ' ');
	    _tokenPtr = 0;

	    while(true) {
		Token token = getToken();
		if (token.getType() == Token::TOKEN_EOF) break;
		cout << token.getToken()<< " : " << token.getType() << " [ " << token.getTokenVal() << " ] " << endl;
	    };

	    return false;
	}

    private:
	string getNextToken() {
	    if (this->_tokenPtr >= this->_tokens.size()) return Token::END;
	    return this->_tokens[this->_tokenPtr++];
	}

	vector<string> _tokens;
	int _tokenPtr;
};




int main() {
    Lexer lexer = Lexer();
    lexer.parse("def extern 10.12");
}
