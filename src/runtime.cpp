#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <llvm/Analysis/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/PassManager.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Analysis/Passes.h>

using namespace std;
using namespace llvm;

class ExprAST;
class PrototypeAST;
class FunctionAST;
class VariantExprAST;

vector<string> split(const string &str, char delim){
  istringstream iss(str); string tmp; vector<string> res;
  while(getline(iss, tmp, delim)) res.push_back(tmp);
  return res;
}

extern "C" 
double putchard(double d) {
  putchar((char)d);
  return 0;
}

extern "C" 
double printd(double d) {
  printf("%f\n", d);
  return 0;
}

class ErrorHandler {
  public:
    static ExprAST* printError(const string& str) { 
      cerr << "\x1b[31m";
      cerr << str << endl;
      cerr << "\x1b[0m";
      return NULL;
    }
    static PrototypeAST* printErrorProto(const string& str) { printError(str); return NULL; }
    static Value* printErrorValue(const string& str) { printError(str); return NULL; }
    static Function* printErrorFunc(const string& str) { printError(str); return NULL; }
};

class Prompt {
  public:
    static void print(string str) {
      cout << "\x1b[32m";
      cout << prmpt << str;
      cout << "\x1b[0m";
    }

    static void println(string str) {
      print(str);
      cout << endl;
    }

  private:
    static const string prmpt;
};
const string Prompt::prmpt = "kaleidoscope> ";

class LLVMIRContext {
  public:
    static Value* getNamedValue(string name) { return _namedValues[name]; }
    static void setNamedValue(string name, Value* value) { _namedValues[name] = value; }
    static void eraseNamedValue(string name) { _namedValues.erase(name);  }
    static void clearNameValues() { _namedValues.clear();  }
    static IRBuilder<>& getBuilder() { return _builder; }
    static Module* getModule() { return _module; }
    static FunctionPassManager* getFPM() { return _fpm; }
    static void setFPM(FunctionPassManager* fpm) { _fpm = fpm; }
    static void enableOptimize(bool enabled = true) { _optimizeEnabled = enabled; }

    static bool optimizeEnabled() { return _optimizeEnabled; }

  private:
    static Module* _module;
    static IRBuilder<> _builder;
    static map<string, Value*> _namedValues;
    static FunctionPassManager* _fpm;
    static bool _optimizeEnabled;
};
IRBuilder<> LLVMIRContext::_builder = IRBuilder<>(getGlobalContext());
Module* LLVMIRContext::_module= new Module("Kaleidoscope", getGlobalContext());
map<string, Value*> LLVMIRContext::_namedValues;
FunctionPassManager* LLVMIRContext::_fpm;
bool LLVMIRContext::_optimizeEnabled = false;

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
    static const string IF;
    static const string THEN;
    static const string ELSE;
    static const string FOR;
    static const string IN;
    static const string UNARY;
    static const string BINARY;
    static const string COMMA;

    static const string EOL;

    enum TokenType {
      TOKEN_EOF = 0, TOKEN_DEF, TOKEN_EXTERN, TOKEN_IDENTIFIER, TOKEN_NUMBER, TOKEN_LBRANCKET, TOKEN_RBRANCKET, TOKEN_BINOP, TOKEN_EOL, 
      TOKEN_IF, TOKEN_THEN, TOKEN_ELSE, TOKEN_FOR, TOKEN_IN, TOKEN_UNARY, TOKEN_BINARY, TOKEN_COMMA,
      TOKEN_OTHERS
    };

    Token () 
      : _token(""), _type(-1)
    {}

    Token(string token) {
      this->_token = token;
      this->_type = TOKEN_OTHERS;
      this->_binop = (char)0;

      if (token == Token::DEF) this->_type = TOKEN_DEF;
      else if (token == Token::EXTERN) this->_type = TOKEN_EXTERN;
      else if (token == Token::LBRANCKET) this->_type = TOKEN_LBRANCKET;
      else if (token == Token::RBRANCKET) this->_type = TOKEN_RBRANCKET;
      else if (token == Token::END) this->_type = TOKEN_EOF;
      else if (token == Token::EOL) this->_type = TOKEN_EOL;
      else if (token == Token::IF) this->_type = TOKEN_IF;
      else if (token == Token::THEN) this->_type = TOKEN_THEN;
      else if (token == Token::ELSE) this->_type = TOKEN_ELSE;
      else if (token == Token::FOR) this->_type = TOKEN_FOR;
      else if (token == Token::IN) this->_type = TOKEN_IN;
      else if (token == Token::UNARY) this->_type = TOKEN_UNARY;
      else if (token == Token::BINARY) this->_type = TOKEN_BINARY;
      else if (token == Token::COMMA) this->_type = TOKEN_COMMA;
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
    //char getBinOp() {return _binop;}
    char getBinOp() {return _token.c_str()[_token.length() - 1];}
    int getType() {return _type;}
    double getTokenVal() {return _tokenVal;}
    bool isAscii() {return _token.length() == 1 && isascii(_token.c_str()[0]);}
    bool isAlNum() {return _token.length() == 1 && isalnum(_token.c_str()[0]);}
    bool isUnary() {return isAscii() && !isAlNum() && _type != Token::TOKEN_LBRANCKET && _type != Token::TOKEN_COMMA;}

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
const string Token::IF= "if";
const string Token::THEN= "then";
const string Token::ELSE= "else";
const string Token::FOR = "for";
const string Token::IN = "in";
const string Token::UNARY = "unary";
const string Token::BINARY = "binary";
const string Token::COMMA = "comma";


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
	  while ((*it) != EOF && (*it) != '\n' && (*it) != '\r') it++;
	  continue;
	}

	if (it != end) {
	  token = *(it++);
	  this->_tokens.push_back(token);
	}
	else break;
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
    virtual Value* codeGen() = 0;
};

class NumberExprAST : public ExprAST {
  public:
    NumberExprAST(double val)
      : _val(val) {}

    Value* codeGen() {
      return ConstantFP::get(getGlobalContext(), APFloat(_val));
    }
  private:
    double _val;
};

class VariantExprAST : public ExprAST {
  public:
    VariantExprAST(string name) 
      : _name(name) {}

    Value* codeGen() {
      Value* v = LLVMIRContext::getNamedValue(this->_name);
      return v ? v : ErrorHandler::printErrorValue("Unknown variable name");
    }
  private:
    string _name;
};

class UnaryExprAST : public ExprAST {
  public:
    UnaryExprAST(char opcode, ExprAST* operand) 
      : _opcode(opcode), _operand(operand) {}

    Value *codeGen() {
      Value* v = this->_operand->codeGen();
      if (!v) return NULL;

      Function *f = LLVMIRContext::getModule()->getFunction(string("unary") + _opcode);

      if (!f)
	return ErrorHandler::printErrorValue("unknown unary operator");

      return LLVMIRContext::getBuilder().CreateCall(f, v, "unop");
    }
  private:
    char _opcode;
    ExprAST *_operand;
};

class BinaryExprAST : public ExprAST {
  public:
    BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs)
      : _op(op), _lhs(lhs), _rhs(rhs) {}

    Value* codeGen() {
      IRBuilder<>& builder = LLVMIRContext::getBuilder();

      Value* l = _lhs->codeGen();
      Value* r = _rhs->codeGen();

      if (!l || !r) return NULL;

      // <result> = <operator> <type> <op1>, <op2>
      switch(_op) {
	case '+' : return builder.CreateFAdd(l, r, "addtmp");
	case '-' : return builder.CreateFSub(l, r, "subtmp");
	case '*' : return builder.CreateFMul(l, r, "multmp");
	case '<' : 
		   l = builder.CreateFCmpULT(l, r, "cmptmp");
		   return builder.CreateUIToFP(l, Type::getDoubleTy(getGlobalContext()), "booltmp");
	default : break; 
      }

      Function* f = LLVMIRContext::getModule()->getFunction(string("binary") + _op);
      if (!f) return ErrorHandler::printErrorValue("undefined binary operator : " + _op);

      Value* ops[2] = { l, r };
      return builder.CreateCall(f, ops, "binop");
    }
  private:
    char _op;
    ExprAST *_lhs, *_rhs;
};

class CallExprAST : public ExprAST {
  public:
    CallExprAST(const string &callee, vector<ExprAST*> &args)
      : _callee(callee), _args(args) {}

    Value* codeGen() {
      Function* calleeF = LLVMIRContext::getModule()->getFunction(_callee);

      if (!calleeF) return NULL;

      if (calleeF->arg_size() != _args.size()) return ErrorHandler::printErrorValue("incorrect # arguments passed");

      vector<Value*> argsV;
      for (vector<ExprAST*>::iterator it = _args.begin(), end = _args.end(); it != end; it++) {
	argsV.push_back((*it)->codeGen());
	if (!argsV.back()) return NULL;
      }

      return LLVMIRContext::getBuilder().CreateCall(calleeF, argsV, "calltmp");
    }
  private:
    string _callee;
    vector<ExprAST*> _args;
};

class PrototypeAST {
  public:
    PrototypeAST(const string &name, const vector<std::string> &args, bool isOperator = false, int prec = 0)
      : _name(name), _args(args), _isOperator(isOperator), _prec(prec) {}

    Function* codeGen() {
      vector<Type*> doubles(_args.size(), Type::getDoubleTy(getGlobalContext()));
      FunctionType* ft = FunctionType::get(Type::getDoubleTy(getGlobalContext()), doubles, false);

      Function* f = Function::Create(ft, Function::ExternalLinkage, _name, LLVMIRContext::getModule());

      if (f->getName() != _name) {
	f->eraseFromParent();
	f = LLVMIRContext::getModule()->getFunction(_name);

	if (!f->empty()) {
	  ErrorHandler::printErrorFunc("redefinition of function.");
	  return NULL;
	}

	if (f->arg_size() != _args.size()) {
	  ErrorHandler::printErrorFunc("redefinition of function with different # args");
	  return NULL;
	}
      }

      unsigned idx = 0;
      for (Function::arg_iterator it = f->arg_begin(); idx != _args.size(); it++, idx++) {
	it->setName(_args[idx]);

	LLVMIRContext::setNamedValue(_args[idx], it);
      }
      return f;
    }

    bool isUnaryOp() { return this->_isOperator && this->_args.size() == 1; }
    bool isBinaryOp() { return this->_isOperator && this->_args.size() == 2; }

    string getName() { return this->_name; }
    char getOpName() { return this->_name.c_str()[this->_name.length()-1]; }
    int getPrecedence() { return this->_prec; }
  private:
    string _name;
    vector<std::string> _args;
    bool _isOperator;
    int _prec;
};

class FunctionAST {
  public:
    FunctionAST(PrototypeAST *proto, ExprAST *body)
      : _proto(proto), _body(body) {}

    Function* codeGen() {
      LLVMIRContext::clearNameValues();

      Function* function = _proto->codeGen();
      if (!function) return NULL;

      BasicBlock* block = BasicBlock::Create(getGlobalContext(), "entry", function);
      LLVMIRContext::getBuilder().SetInsertPoint(block);

      if (Value* ret = _body->codeGen()) {
	LLVMIRContext::getBuilder().CreateRet(ret);
	verifyFunction(*function);
	if (LLVMIRContext::optimizeEnabled()) LLVMIRContext::getFPM()->run(*function);
	return function;
      }

      function->eraseFromParent();
      return NULL;
    }
  private:
    PrototypeAST* _proto;
    ExprAST* _body;
};

class IfExprAST : public ExprAST {
  ExprAST *_cond, *_then, *_else;
  public:
  IfExprAST(ExprAST *cond, ExprAST *then, ExprAST *els)
    : _cond(cond), _then(then), _else(els) {}
  Value *codeGen() {
    Value* condV = _cond->codeGen();
    if (condV == 0) return 0;

    IRBuilder<>& builder = LLVMIRContext::getBuilder();
    condV = builder.CreateFCmpONE(condV,
	ConstantFP::get(getGlobalContext(), APFloat(0.0)),
	"ifcond");

    Function* function = builder.GetInsertBlock()->getParent();

    BasicBlock *thenBlock = BasicBlock::Create(getGlobalContext(), "then", function);
    BasicBlock *elseBlock = BasicBlock::Create(getGlobalContext(), "else");
    BasicBlock *mergeBlock = BasicBlock::Create(getGlobalContext(), "ifcont");

    builder.CreateCondBr(condV, thenBlock, elseBlock);


    builder.SetInsertPoint(thenBlock);

    Value *thenV = _then->codeGen();
    if (thenV == 0) return 0;

    builder.CreateBr(mergeBlock);
    thenBlock = builder.GetInsertBlock();


    function->getBasicBlockList().push_back(elseBlock);
    builder.SetInsertPoint(elseBlock);

    Value *elseV = _else->codeGen();
    if (elseV == 0) return 0;

    builder.CreateBr(mergeBlock);
    elseBlock = builder.GetInsertBlock();


    function->getBasicBlockList().push_back(mergeBlock);
    builder.SetInsertPoint(mergeBlock);
    PHINode *pn = builder.CreatePHI(Type::getDoubleTy(getGlobalContext()), 2, "iftmp");

    pn->addIncoming(thenV, thenBlock);
    pn->addIncoming(elseV, elseBlock);
    return pn;
  }
};

class ForExprAST : public ExprAST {
  public:
    ForExprAST(const string& varName, ExprAST* start, ExprAST* end, ExprAST* step, ExprAST* body)
      : _varName(varName), _start(start), _end(end), _step(step), _body(body)
    {}

    Value* codeGen() {
      Value* startV = this->_start->codeGen();
      if (!startV) return NULL;

      IRBuilder<>& builder = LLVMIRContext::getBuilder();

      Function* function = builder.GetInsertBlock()->getParent();
      BasicBlock* preHeaderBlock = builder.GetInsertBlock();
      BasicBlock* loopBlock = BasicBlock::Create(getGlobalContext(), "loop", function);

      builder.CreateBr(loopBlock);

      builder.SetInsertPoint(loopBlock);

      PHINode* variable = builder.CreatePHI(Type::getDoubleTy(getGlobalContext()), 2, this->_varName.c_str());
      variable->addIncoming(startV, preHeaderBlock);

      Value* oldVal = LLVMIRContext::getNamedValue(this->_varName);
      LLVMIRContext::setNamedValue(this->_varName, variable);

      if (!this->_body->codeGen()) return NULL;

      Value* stepV;
      if (this->_step) {
	stepV = this->_step->codeGen();
	if (!stepV) return NULL;
      } else {
	stepV = ConstantFP::get(getGlobalContext(), APFloat(1.0));
      }

      Value* nextVar = builder.CreateFAdd(variable, stepV, "nextVar");

      Value* endCond = this->_end->codeGen();
      if (!endCond) return NULL;

      endCond = builder.CreateFCmpONE(endCond, ConstantFP::get(getGlobalContext(), APFloat(0.0)), "loopcond");

      BasicBlock* loopEndBlock = builder.GetInsertBlock();
      BasicBlock* afterBlock = BasicBlock::Create(getGlobalContext(), "afterLoop", function); 

      builder.CreateCondBr(endCond, loopBlock, afterBlock);

      builder.SetInsertPoint(afterBlock);

      variable->addIncoming(nextVar, loopEndBlock);
      if (oldVal) LLVMIRContext::setNamedValue(this->_varName, oldVal);
      else LLVMIRContext::eraseNamedValue(this->_varName);

      return Constant::getNullValue(Type::getDoubleTy(getGlobalContext()));
    }
  private:
    string _varName;
    ExprAST *_start, *_end, *_step, *_body;
};

class Parser {
  static const string binOpPrefix;

  public:
  Parser() 
    : _dumpEnabled(false), _optimizeEnabled(false), _dumpOnly(false)
  {
    setPrecedence('<', 10);
    setPrecedence('+', 20);
    setPrecedence('-', 30);
    setPrecedence('*', 40);

    string err;
    _executionEngine = EngineBuilder(LLVMIRContext::getModule()).setErrorStr(&err).create();
    if (!this->_executionEngine) {
      cerr << "could not create ExecutionEngine. : " << err << endl;
      exit(0);
    }

    FunctionPassManager* fpm = new FunctionPassManager(LLVMIRContext::getModule());

    fpm->add(new DataLayout(*(this->_executionEngine->getDataLayout())));
    fpm->add(createBasicAliasAnalysisPass());
    fpm->add(createInstructionCombiningPass());
    fpm->add(createReassociatePass());
    fpm->add(createGVNPass());
    fpm->add(createCFGSimplificationPass());

    fpm->doInitialization();

    LLVMIRContext::setFPM(fpm);
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

    if (_curToken.getToken() != Token::RBRANCKET) return ErrorHandler::printError("expected ')'");
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

	if (_curToken.getToken() != ",") ErrorHandler::printError("Expected ')' or ',' in argument list");
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
      case Token::TOKEN_IF: return parseIf();
      case Token::TOKEN_FOR: return parseFor();
      default: break;
    }
    return ErrorHandler::printError("unknown token when expecting an expression");
  }

  ExprAST* parseExpression() {
    ExprAST* lhs = parseUnary();
    if (!lhs) return NULL;

    return parseBinOpRHS(0, lhs);
  }

  ExprAST* parseBinOpRHS(int exprPrec, ExprAST* lhs) {
    while (1) {
      int prec = getTokPrecedence();

      if (prec < exprPrec) return lhs;
      int binOp = _curToken.getBinOp();

      getNextToken();

      ExprAST* rhs = parseUnary();
      if (!rhs) return 0;

      int nextPrec = getTokPrecedence();
      if (prec < nextPrec) {
	rhs = parseBinOpRHS(prec+1, rhs);
	if (rhs == 0) return 0;
      }
      lhs = new BinaryExprAST(binOp, lhs, rhs);
    }
  }

  ExprAST* parseUnary() {
    if (!this->_curToken.isUnary()) {
      return parsePrimary();
    }

    char op = this->_curToken.getToken().c_str()[0];
    getNextToken();

    if (ExprAST* operand = parseUnary()) {
      return new UnaryExprAST(op, operand);
    }
    return NULL;
  }

  ExprAST* parseIf() {
    getNextToken();  

    ExprAST* cond = parseExpression();
    if (!cond) return NULL;

    if (this->_curToken.getType() != Token::TOKEN_THEN)
      return ErrorHandler::printError("expected then");
    getNextToken(); 

    ExprAST* then = parseExpression();
    if (!then) return NULL;

    if (this->_curToken.getType()!= Token::TOKEN_ELSE)
      return ErrorHandler::printError("expected else");

    getNextToken();

    ExprAST* els = parseExpression();
    if (!els) return NULL;

    return new IfExprAST(cond, then, els);
  }

  ExprAST* parseFor() {
    getNextToken();

    if (this->_curToken.getType() != Token::TOKEN_IDENTIFIER) 
      return ErrorHandler::printError("expected identifier after for");

    string id = this->_curToken.getToken();
    getNextToken();

    if (this->_curToken.getToken() != "=")
      return ErrorHandler::printError("expected '=' after for");

    getNextToken();
    ExprAST* start = parseExpression();
    if (!start) return NULL;

    if (this->_curToken.getToken() != ",") 
      return ErrorHandler::printError("expected ',' after for start value");
    getNextToken();

    ExprAST* end = parseExpression();
    if (!end) return NULL;

    ExprAST* step = NULL;
    if (this->_curToken.getToken() == ",") {
      getNextToken();
      step = parseExpression();
      if (!step) return NULL;
    }

    if (this->_curToken.getType() != Token::TOKEN_IN)
      return ErrorHandler::printError("expected 'in' after for");

    getNextToken();
    ExprAST* body = parseExpression();
    if (!body) return NULL;

    return new ForExprAST(id, start, end, step, body);
  }

  PrototypeAST* parsePrototype() {
    string fnName;
    int binOpPrec = Parser::_defaultPrec;
    int opArgsNum = 0;

    switch (this->_curToken.getType()) {
      case Token::TOKEN_IDENTIFIER:
	fnName = this->_curToken.getToken();
	opArgsNum = 0;
	getNextToken();
	break;
      case Token::TOKEN_BINARY:
	getNextToken();
	fnName = Parser::binOpPrefix;
	fnName += this->_curToken.getToken();
	opArgsNum = 2;
	getNextToken();

	if (this->_curToken.getType() == Token::TOKEN_NUMBER) {
	  binOpPrec = (int)this->_curToken.getTokenVal();
	  getNextToken();
	}
	break;
      case Token::TOKEN_UNARY:
	getNextToken();
	if (!this->_curToken.isAscii())
	  return ErrorHandler::printErrorProto("expected unary operator in prototype");
	fnName = "unary";
	fnName += this->_curToken.getToken();
	opArgsNum = 1;
	getNextToken();
	break;
      default:
	return ErrorHandler::printErrorProto("expected function name in prototype");
    }


    if (this->_curToken.getToken() != Token::LBRANCKET)
      return ErrorHandler::printErrorProto("expected '(' in prototype");

    vector<string> argNames;
    while (getNextToken().getType() == Token::TOKEN_IDENTIFIER)
      argNames.push_back(this->_curToken.getToken());
    if (this->_curToken.getToken() != Token::RBRANCKET)
      return ErrorHandler::printErrorProto("expected ')' in prototype");

    getNextToken();

    if (opArgsNum && argNames.size() != opArgsNum) 
      return ErrorHandler::printErrorProto("invalid number of operands for operator");

    return new PrototypeAST(fnName, argNames, opArgsNum != 0, binOpPrec);
  }

  FunctionAST* parseDefinition() {
    getNextToken();
    PrototypeAST* proto = parsePrototype();
    if (!proto) return NULL;

    if (proto->isBinaryOp()) {
      Parser::setPrecedence(proto->getOpName(), proto->getPrecedence());
    }

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
    if (FunctionAST* ast = parseDefinition()) {
      if (Function* f = ast->codeGen()) {
	if (_dumpEnabled) {
	  Prompt::println("parsed a function definition.");
	  f->dump();
	}
      }
    } else {
      getNextToken();
    }
  }

  void handleExtern() {
    if (PrototypeAST* ast = parseExtern()) {
      if (Function* f = ast->codeGen()) {
	if (_dumpEnabled) {
	  Prompt::println("parsed an extern.");
	  f->dump();
	}
      }
    } else {
      getNextToken();
    }
  }

  void handleTopLevelExpression() {
    if (FunctionAST* ast = parseTopLevelExpr()) {
      if (Function* f = ast->codeGen()) {
	if (_dumpEnabled) {
	  Prompt::println("parsed a top-level expr.");
	  f->dump();
	}

	if (!_dumpOnly) {
	  void* fptr = _executionEngine->getPointerToFunction(f);
	  double (*fp)() = (double (*)())(intptr_t)fptr;
	  stringstream ss;
	  ss << fp();
	  Prompt::println(ss.str());
	}
      }
    } else {
      getNextToken();
    }
  } 

  void parse(const string& input) {
    this->_lexer = Lexer(input);
    getNextToken();

    while (!isEmpty()) {
      switch(this->_curToken.getType()) {
	case Token::TOKEN_EOF: return;
	case Token::TOKEN_EOL: getNextToken(); break;
	case Token::TOKEN_DEF: handleDefinition(); break;
	case Token::TOKEN_EXTERN: handleExtern(); break;
	default: handleTopLevelExpression(); break;
      }
    }
  }

  void enableDump(bool enabled = true) { _dumpEnabled = enabled; }
  void enableDumpOnly(bool enabled = true) { _dumpOnly = enabled; }

  bool isEmpty() { return this->_curToken.getType() == Token::TOKEN_EOF; }

  void setPrecedence(char op, int prec) { Parser::_precedence[op] = prec; }
  int getPrecedence(char op) { return Parser::_precedence[op]; }
  private:
  Token getNextToken() { return (this->_curToken = this->_lexer.getToken()); }

  int getTokPrecedence() {
    char op = this->_curToken.getBinOp();
    if (!isascii(op)) return -1;

    int prec = this->_precedence[op];
    if (prec <= 0) return -1;
    return prec;
  }

  Lexer _lexer;
  Token _curToken;
  static map<char, int> _precedence;
  ExecutionEngine* _executionEngine;
  bool _dumpOnly;
  bool _dumpEnabled;
  bool _optimizeEnabled;

  static int _defaultPrec;
};

int Parser::_defaultPrec = 30;
const string Parser::binOpPrefix = "binary";
map<char, int> Parser::_precedence;

class CommandLine {
  public:
    static map<string, string> parse(int argc, char* argv[]) {
      map<string, string> args;

      for (int i=1; i<argc; i++) {
	vector<string> kv = split(argv[i], '=');
	if (kv.size() == 2) args[kv[0]] = kv[1];
	if (kv.size() == 1) args[kv[0]] = "1";
      }

      return args;
    }
};

void runInterpreter(Parser& parser) {
  string input;

  while (true) {
    Prompt::print("");
    getline(cin, input);
    if (input == "exit") break;
    parser.parse(input);
  }
}

int main(int argc, char* argv[]) {
  InitializeNativeTarget();

  map<string, string> args = CommandLine::parse(argc, argv);

  string in = args["-i"];
  string out = args["-o"];
  bool dumpOnly = args["--dump-only"] == "1";
  bool dumpEnabled = args["--dump-enabled"] == "1";
  bool optimizeEnabled = args["--opt-enabled"] == "1";

  if (dumpOnly) dumpEnabled = true;

  Parser parser;
  parser.enableDump(dumpEnabled);
  parser.enableDumpOnly(dumpOnly);
  LLVMIRContext::enableOptimize(optimizeEnabled);

  if (!in.empty())  cout << "input : " + in << endl;
  if (!out.empty()) cout << "output : " + out << endl;
  if (dumpOnly)  cout << "dump only." << endl;
  if (dumpEnabled)  cout << "dump enabled." << endl;
  if (optimizeEnabled)  cout << "optimize enabled." << endl;

  if (in.empty()) {
    runInterpreter(parser);
  }
  else {
    string input;

    ifstream ifs(in.c_str());
    if (!ifs) {
      cerr << "failed open file : " + in << endl;
      exit(0);
    }

    string buf;
    while (getline(ifs, buf)) input += buf + '\n';
    parser.parse(input);

    if (!out.empty()) {
      FILE* fp = freopen(out.c_str(), "w", stderr);
      if (!fp) {
	cerr << "failed open file : " + out << endl;
	exit(0);
      }

      LLVMIRContext::getModule()->dump();
      fclose(fp);
    }
  }
}
