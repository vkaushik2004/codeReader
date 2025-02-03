
#include <cstdlib>
#include <iostream>
#include <map>
#include <stdarg.h>
#include <stdlib.h>


#include "compiler.h"
#include "lexer.h"


using namespace std;
// instantiate bunch of stuff and functions to use later

LexicalAnalyzer lexer;
Token tok;

// map where key is the tok.lexme and int is value
map<string, int> input;

// instantiate main helper functions for parsting (the functions that generate
// nodes for the main reqs of project)
InstructionNode *pSwitch(struct InstructionNode *switchStmt);
InstructionNode *pFor();
InstructionNode *pOutput();
InstructionNode *pInput();
InstructionNode *pWhile();
InstructionNode *pIf();
InstructionNode *PAssStmt();

// secondary  helpers (stuff made to help finish main tasks and organize stuff)

// used for switch
InstructionNode *pCaseList(int operation, struct InstructionNode *CList);
InstructionNode *pCase(int operation);
InstructionNode *pDefault();
// used for assignment
InstructionNode *PExp();
// used for beginning and end for inputs and variable lists
void parseIdList();
void parseInput();
// miscellaneous (used anywhere)
int primParse();
InstructionNode *PBody();
InstructionNode *PStmtList();
InstructionNode *PStmt();
InstructionNode *pCond();

struct InstructionNode *instruction;

// j an error built in for expect func
void error() {
  cout << "SYNTAX ERROR !!!\n";
  exit(1);
}

// utilize expect to ensure that the code we are reading works properly
// since we know exactly how functions will be defined we can use this to check
// if we can move forward in instruction node creation
Token shouldBe(TokenType expected_type) {
  Token t = lexer.GetToken();
  if (t.token_type != expected_type)
    error();
  return t;
}

struct InstructionNode *parse_generate_intermediate_representation() {
  // section 1 with all variables to be used in program
  tok = lexer.peek(1);
  if (tok.token_type == ID) {
    // id_list
    parseIdList();
    shouldBe(SEMICOLON);
  } 
  // section 2 acc code
  tok = lexer.peek(1);
  if (tok.token_type == LBRACE) {
    instruction = PBody();
  }
  // section 3 inputs to put into inputs vector
  tok = lexer.peek(1);
  if (tok.token_type == NUM) {
    parseInput();
  } 
  return instruction; // returning head instruction of list
}


// helper called in generate
void parseIdList() {
  tok = shouldBe(ID);
  // store vare to map and value is index in mem
  input[tok.lexeme] = next_available;
  // make val 0 in mem since vars are assumed to be 0 before ASSIGN or INPUT
  // inst
  mem[next_available++] = 0;

  tok = lexer.GetToken();
  // recursive call
  if (tok.token_type == COMMA) {
    parseIdList();
  } else {
    lexer.UngetToken(1);
  }
}

// helper called in generate
struct InstructionNode *PBody() {
  struct InstructionNode *instl = nullptr;
  shouldBe(LBRACE);
  // stmt_list
  instl = PStmtList();
  shouldBe(RBRACE);
  return instl;
}

// recursively parse through a list of num
void parseInput() {
  tok = shouldBe(NUM);
  // append input number
  inputs.push_back(stoi(tok.lexeme));
  // num_list
  tok = lexer.peek(1);
  if (tok.token_type == NUM) {
    parseInput();
  }
}

struct InstructionNode *PStmtList() {
  // have 2 nodes
  struct InstructionNode *inst = nullptr;
  struct InstructionNode *instl = nullptr;
  TokenType debug1 = tok.token_type;
  string debug2 = tok.lexeme;
  // call parse statement
  inst = PStmt();
  tok = lexer.peek(
      1); // peek to next statement and see what the next stattement type is
  if (tok.token_type == ID || tok.token_type == WHILE || tok.token_type == IF ||
      tok.token_type == SWITCH || tok.token_type == FOR || tok.token_type == OUTPUT ||
      tok.token_type == INPUT) {
    instl = PStmtList();

    // assign instl to the tail of inst
    struct InstructionNode *getLast = inst;
    while (getLast->next != nullptr) {
      getLast = getLast->next;
    }
    getLast->next = instl;
  }
  return inst;
}

// helper for parsing a given statement
struct InstructionNode *PStmt() {
  struct InstructionNode *inst = nullptr;
  struct InstructionNode *switchNode = new InstructionNode;
  switchNode->type = NOOP;
  switchNode->next = nullptr;
  struct InstructionNode *getLast;
  tok = lexer.peek(1);
  switch (tok.token_type) {
  // depending on case of statement call dif helper
    case OUTPUT:
    inst = pOutput();
    break;

  case INPUT:
    inst = pInput();
    break;

  case ID:
    inst = PAssStmt();
    break;

  case IF:
    inst = pIf();
    break;

    case WHILE:
    inst = pWhile();
    break;

  case SWITCH:
    inst = pSwitch(switchNode);
    // same logic as in PStmtList
    getLast = inst;
    while (getLast->next != nullptr) {
      getLast = getLast->next;
    }
    getLast->next = switchNode;
    break;

  case FOR:
    inst = pFor();
    break;

  default:
    return nullptr;
  }

  return inst;
}

// get input info from input vector
struct InstructionNode *pInput() {
  auto *inputInst = new InstructionNode;
  shouldBe(INPUT);
  //set node type and array val in mem 
  inputInst->type = IN;
  tok = shouldBe(ID);
  inputInst->input_inst.var_index = input[tok.lexeme];
  inputInst->next = nullptr;
  shouldBe(SEMICOLON); // end of input 
  return inputInst;
}

// node for printing output
struct InstructionNode *pOutput() {
  auto *outputInst = new InstructionNode;
  shouldBe(OUTPUT);
  //set type and grab array val in mem
  outputInst->type = OUT;
  tok = shouldBe(ID);
  outputInst->output_inst.var_index = input[tok.lexeme];
  outputInst->next = nullptr;
  shouldBe(SEMICOLON);
  return outputInst;
}

// assignment can either change mem value directly or call for an op to be done
// on 2 operands
struct InstructionNode *PAssStmt() {
  auto *assignInst = new InstructionNode;
  assignInst->type = ASSIGN;
  // left hand side
  tok = shouldBe(ID);
  // pull LHS from input vector and assign to instruction node
  assignInst->assign_inst.left_hand_side_index = input[tok.lexeme];

  shouldBe(EQUAL);

  // can b assigned to a NUM or an expr
  Token currTok = lexer.GetToken();
  Token peekTok = lexer.peek(1);
  lexer.UngetToken(1);
  if (currTok.token_type == ID || currTok.token_type == NUM) {
    // if token two is operator then we also call parse ecponenent
    if (peekTok.token_type == MINUS || peekTok.token_type == PLUS ||
        peekTok.token_type == MULT || peekTok.token_type == DIV) {
      // expression node to append new operands
      struct InstructionNode *expNode = PExp();
      //set what we can from the helper func
      assignInst->assign_inst.operand1_index = expNode->assign_inst.operand1_index;
      assignInst->assign_inst.op = expNode->assign_inst.op;
      assignInst->assign_inst.operand2_index =expNode->assign_inst.operand2_index;
    } else if (peekTok.token_type == SEMICOLON) {
      // if no operator op none
      assignInst->assign_inst.op = OPERATOR_NONE;
      assignInst->assign_inst.operand1_index = primParse();
    } 
  }
  shouldBe(SEMICOLON);
  // set next to null
  assignInst->next = nullptr;
  // return whichever assignment node was created NONE or the ones with OPs
  return assignInst;
}

// this helper just assigns the values of the assn isntruction node that define
// the operands and operator
struct InstructionNode *PExp() {
  auto *getInfo = new InstructionNode;
  getInfo->assign_inst.operand1_index = primParse();
  tok = lexer.GetToken();

  switch (tok.token_type) {
  case PLUS:
    getInfo->assign_inst.op = OPERATOR_PLUS;
    break;
  
  case MINUS:
    getInfo->assign_inst.op = OPERATOR_MINUS;
    break;
    
  case DIV:
    getInfo->assign_inst.op = OPERATOR_DIV;
    break;
  
  case MULT:
    getInfo->assign_inst.op = OPERATOR_MULT;
    break;

  default:
    break;
  }
  getInfo->assign_inst.operand2_index = primParse();

  return getInfo;
}

// helper that returns the mem[] value of the token it runs into during
// expressions
int primParse() {
  int index = -1;
  tok = lexer.GetToken();
  if (tok.token_type == ID || tok.token_type == NUM) {
    if (tok.token_type == ID) {
      index = input[tok.lexeme];
    } else {
      index = next_available;
      mem[next_available++] = stoi(tok.lexeme);
    }
  } 
  return index;
}

struct InstructionNode *pIf() {
  auto *ifInst = new InstructionNode;
  shouldBe(IF);
  // set everything we can rn
  ifInst->type = CJMP;
  struct InstructionNode *temp = pCond();
  ifInst->cjmp_inst.operand1_index = temp->cjmp_inst.operand1_index;
  ifInst->cjmp_inst.condition_op = temp->cjmp_inst.condition_op;
  ifInst->cjmp_inst.operand2_index = temp->cjmp_inst.operand2_index;
  // peek so we can find the next body to parse
  tok = lexer.peek(1);
  if (tok.token_type == LBRACE) {
    ifInst->next = PBody();
  } 
  // NOOP for end of if to jump to
  auto *noop = new InstructionNode;
  noop->type = NOOP;
  noop->next = nullptr;

  // get to end of linked list
  struct InstructionNode *getLast = ifInst;
  while (getLast->next != nullptr) {
    getLast = getLast->next;
  }
  // last is set to noop
  getLast->next = noop;

  // target is also set to noop
  ifInst->cjmp_inst.target = noop;

  return ifInst;
}

struct InstructionNode *pWhile() {
  // j like if set what values we can
  auto *whileInst = new InstructionNode;
  shouldBe(WHILE);
  whileInst->type = CJMP; // while is type sjmp
  struct InstructionNode *condition = pCond();
  whileInst->cjmp_inst.operand1_index = condition->cjmp_inst.operand1_index;
  whileInst->cjmp_inst.condition_op = condition->cjmp_inst.condition_op;
  whileInst->cjmp_inst.operand2_index = condition->cjmp_inst.operand2_index;
  // j like if find next body to pars
  tok = lexer.peek(1);
  if (tok.token_type == LBRACE) {
    whileInst->next = PBody();
  } 

  // difference here is we have a jump here now and a noop
  auto *jmp = new InstructionNode;
  jmp->type = JMP;
  jmp->jmp_inst.target = whileInst;

  auto *noop = new InstructionNode;
  noop->type = NOOP;
  noop->next = nullptr;

  // j like if get to end
  struct InstructionNode *getLast = whileInst;
  while (getLast->next != nullptr) {
    getLast = getLast->next;
  }
  // set last items next to jmp the jmps next to noop and the whiles target to
  // noop
  getLast->next = jmp;
  jmp->next = noop;
  whileInst->cjmp_inst.target = noop;

  return whileInst;
}

// helper to break down conditionals
struct InstructionNode *pCond() {
  auto *getCondInfo = new InstructionNode;
  // primary
  tok = lexer.peek(1);
  if (tok.token_type == ID || tok.token_type == NUM) {
    getCondInfo->cjmp_inst.operand1_index = primParse();
  } 
  tok = lexer.peek(1);
  if (tok.token_type == GREATER || tok.token_type == LESS ||
      tok.token_type == NOTEQUAL) {
    tok = lexer.GetToken(); // next token should be the condition type
    switch (tok.token_type) {
    
    case LESS:
      getCondInfo->cjmp_inst.condition_op = CONDITION_LESS;
      break;
    
    case GREATER:
      getCondInfo->cjmp_inst.condition_op = CONDITION_GREATER;
      break;
    
    case NOTEQUAL:
      getCondInfo->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
      break;
    
    default:
      break;
    }
  } 
  // primary
  tok = lexer.peek(1);
  if (tok.token_type == ID || tok.token_type == NUM) {
    getCondInfo->cjmp_inst.operand2_index = primParse();
  } 
  return getCondInfo;
}

struct InstructionNode *pFor() {
  // we know fors have assignment instructions so we make one here
  auto *forInst = new InstructionNode;
  auto *assignStmt = new InstructionNode;
  shouldBe(FOR);
  shouldBe(LPAREN);

  // assign_stmt
  tok = lexer.peek(1);
  if (tok.token_type == ID) {
    forInst = PAssStmt();
  } 

  // for loops break down as whiles with Cjmps so make a while here
  auto *tempWhileInst = new InstructionNode;
  tempWhileInst->type = CJMP;
  // condition
  struct InstructionNode *cond = pCond();
  // define what we can which is the conditional
  tempWhileInst->cjmp_inst.operand1_index = cond->cjmp_inst.operand1_index;
  tempWhileInst->cjmp_inst.condition_op = cond->cjmp_inst.condition_op;
  tempWhileInst->cjmp_inst.operand2_index = cond->cjmp_inst.operand2_index;

  shouldBe(SEMICOLON);

  tok = lexer.peek(1);
  // assignment
  if (tok.token_type == ID) {
    assignStmt = PAssStmt();
    assignStmt->next = nullptr;
    shouldBe(RPAREN);
  }
  // body
  tok = lexer.peek(1);
  if (tok.token_type == LBRACE) {
    tempWhileInst->next = PBody();
  } 
  // add the 2nd stmt
  auto *addStmt = tempWhileInst->next;
  while (addStmt->next != nullptr) {
    addStmt = addStmt->next;
  }
  addStmt->next = assignStmt;

  // set NOOP type and target to null
  auto *noop = new InstructionNode;
  noop->type = NOOP;
  noop->next = nullptr;

  // set jmp type and its target
  auto *jmp = new InstructionNode;
  jmp->type = JMP;
  jmp->jmp_inst.target = tempWhileInst;

  // jmp node goes to noop
  jmp->next = noop;

  // get to end of list
  struct InstructionNode *getLast = tempWhileInst;
  while (getLast->next != nullptr) {
    getLast = getLast->next;
  }
  // last node goes to jmp now and the target of temp while is noop
  getLast->next = jmp;
  tempWhileInst->cjmp_inst.target = noop;
  // now for next is set to temp while
  forInst->next = tempWhileInst;

  return forInst;
}

struct InstructionNode *pSwitch(struct InstructionNode *switchInstruction) {
  auto *switchInst = new InstructionNode;
  shouldBe(SWITCH);

  // after a switch should have an ID type so set that to op1 of switch
  // instruction
  tok = shouldBe(ID);
  int switchOp1 = input[tok.lexeme];
  shouldBe(LBRACE);
  // case list
  tok = lexer.peek(1);
  if (tok.token_type == CASE) {
    switchInst = pCaseList(switchOp1, switchInst);
  } 
  // default
  tok = lexer.peek(1);
  if (tok.token_type == DEFAULT) {

    struct InstructionNode *getLast = switchInst;
    // get to end
    while (getLast->next->next != nullptr) {
      getLast = getLast->next;
    }
    getLast->next = pDefault();
    // should be an right brace to signify end of block
    shouldBe(RBRACE);
  } else if (tok.token_type == RBRACE) {
    tok = lexer.GetToken();
    return switchInst;
  } 
  return switchInst;
}

struct InstructionNode *pCaseList(int op, struct InstructionNode *caseStart) {
  auto *caseNode = new InstructionNode;
  struct InstructionNode *caseList = nullptr;
  // case
  tok = lexer.peek(1);
  if (tok.token_type == CASE) {
    caseNode = pCase(op);

    auto *jmp = new InstructionNode;
    jmp->type = JMP;
    jmp->jmp_inst.target = caseStart;
    struct InstructionNode *getLast = caseNode->cjmp_inst.target;
    while (getLast->next->next != nullptr) {
      getLast = getLast->next;
    }
    getLast->next = jmp;

  } 
  // case_list
  tok = lexer.peek(1);
  if (tok.token_type == CASE) {
    caseList = pCaseList(op, caseStart);

    // add caseList to the tail of caseNode
    struct InstructionNode *getLast = caseNode;
    while (getLast->next->next != nullptr) {
      getLast = getLast->next;
    }
    getLast->next = caseList;
  }
  return caseNode;
}

struct InstructionNode *pCase(int op) {
  // each case is essentially an if statement so we set every case as CJMP
  auto *caseInst = new InstructionNode;
  shouldBe(CASE);
  caseInst->type = CJMP;
  caseInst->cjmp_inst.operand1_index = op;
  caseInst->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
  tok = shouldBe(NUM);

  // add the NUM to mem
  int index = next_available;
  mem[next_available++] = stoi(tok.lexeme);
  // assign the num to operand2
  caseInst->cjmp_inst.operand2_index = index;

  shouldBe(COLON);
  // body
  tok = lexer.peek(1);
  if (tok.token_type == LBRACE) {
    caseInst->cjmp_inst.target = PBody();
  } 

  // create noop for the target of case jump
  auto *noop = new InstructionNode;
  noop->type = NOOP;
  noop->next = nullptr;

  struct InstructionNode *getLast = caseInst->cjmp_inst.target;
  // hit end of linked list
  while (getLast->next != nullptr) {
    getLast = getLast->next;
  }
  caseInst->next = noop;
  getLast->next = caseInst->next;
  return caseInst;
}

// when we are in switch we call this to parse the default list
struct InstructionNode *pDefault() {
  auto *defaultInst = new InstructionNode;
  shouldBe(DEFAULT);
  shouldBe(COLON);
  // body
  tok = lexer.peek(1);
  if (tok.token_type == LBRACE) {
    defaultInst = PBody();
  } 
  return defaultInst;
}


