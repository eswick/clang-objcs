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

namespace CodeGen {
  class CodeGenFunction;
  class CodeGenModule;

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
};

CGObjCSRuntime *CreateMobileSubstrateObjCSRuntime(CodeGenModule &CGM);
}
}
