//===----- CGObjCSRuntime.h - Interface to ObjCS Runtimes -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This provides an abstract class for Objective-CS code generation.  Concrete
// subclasses of this implement code generation for specific Objective-CS
// runtime libraries.
//
//===----------------------------------------------------------------------===//

#include <vector>

namespace llvm {
  class Function;
}

namespace clang {

  class ObjCMethodDecl;
  class ObjCHookDecl;
  class ObjCOrigExpr;

namespace CodeGen {
  class CodeGenFunction;
  class CodeGenModule;
  class RValue;
  class ReturnValueSlot;

class CGObjCSRuntime {
protected:
  CodeGen::CodeGenModule &CGM;
  CGObjCSRuntime(CodeGen::CodeGenModule &CGM) : CGM(CGM) {}

public:
  virtual ~CGObjCSRuntime();
  
  virtual void GenerateMethodHook(CodeGenFunction &CGF,
                                  const ObjCMethodDecl *OMD,
                                  const ObjCHookDecl *HD) = 0;

  virtual void GenerateHookConstructor(CodeGenFunction &CGF,
                                       ObjCHookDecl *HD) = 0;

  std::vector<llvm::Function*> HookConstructors;
  
  virtual RValue GenerateOrigExpr(CodeGenFunction &CGF,
                                  const ObjCOrigExpr *E,
                                  ReturnValueSlot Return) = 0;
};

CGObjCSRuntime *CreateMobileSubstrateObjCSRuntime(CodeGenModule &CGM);
}
}
