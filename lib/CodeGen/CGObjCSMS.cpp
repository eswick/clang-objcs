//===- CGObjCSMS.cpp - Interface to MobileSubstrate Objective-CS Runtime --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This provides Objective-CS code generation targeting MobileSubstrate
//
//===----------------------------------------------------------------------===//

#include "CGObjCSRuntime.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/DenseMap.h"

using namespace clang;
using namespace CodeGen;

namespace {

class CGObjCSMS : public CGObjCSRuntime {
private:
  
  /// Create the mangled  name for a method hook
  ///
  /// methodhook$<class>$<selector>
  ///
  /// Semicolons (:) in selector are replaced with '$'
  void getMangledNameForMethodHook(const ObjCMethodDecl *D, SmallVectorImpl<char> &Name) {
      llvm::raw_svector_ostream OS(Name);
      
      std::string sel = D->getSelector().getAsString();
      std::replace(sel.begin(), sel.end(), ':', '$');
      
      OS << "methodhook$" << D->getClassInterface()->getName() << "$" << sel;
  } 

public:
  CGObjCSMS(CodeGen::CodeGenModule &cgm) : CGObjCSRuntime(cgm) { }
  
  void GenerateMethodHook(CodeGenFunction &CGF,
                          const ObjCMethodDecl *OMD,
                          const ObjCHookDecl *HD) override {
                            SmallString<256> Name;
    getMangledNameForMethodHook(OMD, Name);

    // Set up LLVM types
    CodeGenTypes &Types = CGF.getTypes();
    
    llvm::FunctionType *MethodTy = Types.GetFunctionType(
                                       Types.arrangeObjCMethodDeclaration(OMD));
    llvm::Function *Fn = llvm::Function::Create(MethodTy, 
                                            llvm::GlobalValue::InternalLinkage, 
                                            Name.str(),
                                            &CGM.getModule());
    
    const CGFunctionInfo &FI = Types.arrangeObjCMethodDeclaration(OMD);
    CGM.SetInternalFunctionAttributes(OMD, Fn, FI);
    
    
    // Create function args (self, _cmd, ...)
    FunctionArgList args;
    args.push_back(OMD->getSelfDecl());
    args.push_back(OMD->getCmdDecl());
    
    args.append(OMD->param_begin(), OMD->param_end());
    
    CGF.CurGD = OMD;
    
    // Emit method
    CGF.StartFunction(OMD, OMD->getReturnType(), Fn, FI, args, OMD->getLocStart());
    CGF.EmitCompoundStmtWithoutScope(*cast<CompoundStmt>(OMD->getBody()));
    CGF.FinishFunction(OMD->getBodyRBrace());
  }
  
};
}

CGObjCSRuntime* CodeGen::CreateMobileSubstrateObjCSRuntime(CodeGenModule &CGM) {
  return new CGObjCSMS(CGM);
}