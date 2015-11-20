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
#include "CGObjCRuntime.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CallSite.h"

using namespace clang;
using namespace CodeGen;

namespace {

class CGObjCSMS : public CGObjCSRuntime {
private:
  /// MethodDefinitions - map of methods which have been defined in
  /// this hook.
  llvm::DenseMap<const ObjCMethodDecl*, llvm::Function*> MethodDefinitions;

  void setMethodDefinition(const ObjCMethodDecl *OMD, llvm::Function *Fn) {
    MethodDefinitions.insert(std::make_pair(OMD, Fn));
  }

  llvm::Function* getMethodDefinition(const ObjCMethodDecl *OMD) {
    llvm::DenseMap<const ObjCMethodDecl*,
                   llvm::Function*>::iterator I = MethodDefinitions.find(OMD);
    if (I != MethodDefinitions.end())
      return I->second;

    return NULL;
  }
  
  llvm::DenseMap<const ObjCMethodDecl*, llvm::GlobalVariable*> OrigPointers;
  
  void setOrigPointer(const ObjCMethodDecl *OMD, llvm::GlobalVariable *Fn) {
    OrigPointers.insert(std::make_pair(OMD, Fn));
  }

  llvm::GlobalVariable* getOrigPointer(const ObjCMethodDecl *OMD) {
    llvm::DenseMap<const ObjCMethodDecl*, llvm::GlobalVariable*>::iterator I = 
                                                       OrigPointers.find(OMD);
    if (I != OrigPointers.end())
      return I->second;

    return NULL;
  }

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
  
  void getMangledNameForOrigPointer(const ObjCMethodDecl *D, 
                                    SmallVectorImpl<char> &Name) {
      llvm::raw_svector_ostream OS(Name);

      std::string sel = D->getSelector().getAsString();
      std::replace(sel.begin(), sel.end(), ':', '$');

      OS << "origpointer$" << D->getClassInterface()->getName() << "$" << sel;
  }

  llvm::Function* StartHookConstructor(CodeGenFunction &CGF) {
    FunctionArgList args; // Arguments for the ctor (there are none)

    llvm::FunctionType *MethodTy = llvm::FunctionType::get(CGF.VoidTy, false);
    llvm::Function *Fn = llvm::Function::Create(MethodTy,
                                             llvm::GlobalValue::InternalLinkage,
                                             StringRef("objcsHookInit"),
                                             &CGM.getModule());

    CodeGenTypes &Types = CGF.getTypes();
    CGF.CurFnInfo = &Types.arrangeNullaryFunction();

    // Write ctor prologue
    llvm::BasicBlock *EntryBB = CGF.createBasicBlock("entry", Fn);

    llvm::Value *Undef = llvm::UndefValue::get(CGF.Int32Ty);
    CGF.AllocaInsertPt = new llvm::BitCastInst(Undef, CGF.Int32Ty,
                                               "", EntryBB);
    if (CGF.Builder.isNamePreserving())
      CGF.AllocaInsertPt->setName("allocapt");

    CGF.ReturnBlock = CGF.getJumpDestInCurrentScope("return");

    CGF.Builder.SetInsertPoint(EntryBB);

    CGF.ReturnValue = 0;

    CGF.EmitStartEHSpec(CGF.CurCodeDecl);

    CGF.PrologueCleanupDepth = CGF.EHStack.stable_begin();
    CGF.EmitFunctionProlog(*CGF.CurFnInfo, Fn, args);

    return Fn;
  }

  llvm::CallInst* EmitGetClassRuntimeCall(CodeGenFunction &CGF,
                                          std::string className) {
    llvm::Type *objc_getClassArgTypes[] = { CGF.Int8PtrTy };
    llvm::FunctionType *objc_getClassType =
                                  llvm::FunctionType::get(CGF.Int8PtrTy,
                                                          objc_getClassArgTypes,
                                                          false);

    llvm::Constant *objc_getClassFn =
                                    CGM.CreateRuntimeFunction(objc_getClassType,
                                                              "objc_getClass");

    if (llvm::Function *f = dyn_cast<llvm::Function>(objc_getClassFn)) {
        f->setLinkage(llvm::Function::ExternalWeakLinkage);
    }

    llvm::Constant *classString = CGM.GetAddrOfConstantCString(className);

    llvm::Value *objc_getClassArgs[1];
    objc_getClassArgs[0] = llvm::ConstantExpr::getBitCast(classString,
                                                          CGF.Int8PtrTy);

    return CGF.EmitNounwindRuntimeCall(objc_getClassFn, objc_getClassArgs);
  }

  llvm::CallInst* EmitGetMetaclassRuntimeCall(CodeGenFunction &CGF,
                                              llvm::CallInst *clazz) {
    llvm::Type *object_getClassArgTypes[] = { CGF.Int8PtrTy };
    llvm::FunctionType *object_getClassType =
                                llvm::FunctionType::get(CGF.Int8PtrTy,
                                                        object_getClassArgTypes,
                                                        false);

    llvm::Constant *object_getClassFn =
                                  CGM.CreateRuntimeFunction(object_getClassType,
                                                            "object_getClass");

    if (llvm::Function *f = dyn_cast<llvm::Function>(object_getClassFn)) {
        f->setLinkage(llvm::Function::ExternalWeakLinkage);
    }

    llvm::Value *object_getClassArgs[1];
    object_getClassArgs[0] = clazz;

    return CGF.EmitNounwindRuntimeCall(object_getClassFn, object_getClassArgs);
  }

/// Emits a call to MSHookMessageEx with the given class, message, and hook.
/// old should be a pointer to a function pointer that will point to the
/// original method after the hook is complete.

  void EmitMessageHookExRuntimeCall(CodeGenFunction &CGF,
                                    llvm::CallInst *_class,
                                    llvm::Value *message,
                                    llvm::Function* hook,
                                    llvm::Value *old) {

    llvm::Type *Int8PtrTy = CGF.Int8PtrTy;

    llvm::Type *msgHookExArgTypes[] = { Int8PtrTy, Int8PtrTy,
                                        Int8PtrTy, Int8PtrTy };
    llvm::FunctionType *msgHookExType =
                                llvm::FunctionType::get(CGF.Builder.getVoidTy(),
                                                        msgHookExArgTypes,
                                                        false);

    llvm::Constant *msHookMsgExFn = CGM.CreateRuntimeFunction(msgHookExType,
                                                             "MSHookMessageEx");

    if (llvm::Function *f = dyn_cast<llvm::Function>(msHookMsgExFn)) {
      f->setLinkage(llvm::Function::ExternalWeakLinkage);
    }

    llvm::Value *msHookMsgExArgs[4];
    msHookMsgExArgs[0] = CGF.Builder.CreateBitCast(_class, Int8PtrTy);
    msHookMsgExArgs[1] = CGF.Builder.CreateBitCast(message, Int8PtrTy);
    msHookMsgExArgs[2] = CGF.Builder.CreateBitCast(hook, Int8PtrTy);
    msHookMsgExArgs[3] = CGF.Builder.CreateBitCast(old, Int8PtrTy);

    CGF.EmitRuntimeCallOrInvoke(msHookMsgExFn, msHookMsgExArgs);
  }
  
  void EmitNewMethod(CodeGenFunction &CGF, llvm::CallInst *_class,
                     llvm::Value *selector, llvm::Function *impl,
                     ObjCMethodDecl *OMD) {

    // Generate string constant for method type encoding
    std::string TypeStr;
    CGF.getContext().getObjCEncodingForMethodDecl(OMD, TypeStr);
    llvm::Constant *typeEncoding = CGM.GetAddrOfConstantCString(TypeStr);
    
    llvm::Type *Int8PtrTy = CGF.Int8PtrTy;
    
    llvm::Type *classAddMethodArgTypes[] = { Int8PtrTy, Int8PtrTy, 
                                             Int8PtrTy, Int8PtrTy };
    llvm::FunctionType *classAddMethodType = llvm::FunctionType::get(
                                                        CGF.Builder.getVoidTy(),
                                                        classAddMethodArgTypes,
                                                        false);
    llvm::Constant *classAddMethodFn = CGM.CreateRuntimeFunction(
                                                            classAddMethodType,
                                                            "class_addMethod");

    if (llvm::Function *f = dyn_cast<llvm::Function>(classAddMethodFn)) {
      f->setLinkage(llvm::Function::ExternalWeakLinkage);
    }
    
    llvm::Value *classAddMethodArgs[4];
    classAddMethodArgs[0] = CGF.Builder.CreateBitCast(_class, Int8PtrTy);
    classAddMethodArgs[1] = CGF.Builder.CreateBitCast(selector, Int8PtrTy);
    classAddMethodArgs[2] = CGF.Builder.CreateBitCast(impl, Int8PtrTy);
    classAddMethodArgs[3] = CGF.Builder.CreateBitCast(typeEncoding, Int8PtrTy);
    
    CGF.EmitRuntimeCallOrInvoke(classAddMethodFn, classAddMethodArgs);
  }

public:
  CGObjCSMS(CodeGen::CodeGenModule &cgm) : CGObjCSRuntime(cgm) { }

  void GenerateMethodHook(CodeGenFunction &CGF,
                          const ObjCMethodDecl *OMD,
                          const ObjCHookDecl *HD) override {
    SmallString <256> OrigName;
    getMangledNameForOrigPointer(OMD, OrigName);

    llvm::GlobalVariable *Orig;

    Orig = new llvm::GlobalVariable(
      CGM.getModule(),
      CGF.Int8PtrTy,
      false,
      llvm::GlobalValue::InternalLinkage,
      CGM.EmitNullConstant(CGF.getContext().VoidPtrTy),
      OrigName.str());

    setOrigPointer(OMD, Orig);

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

    setMethodDefinition(OMD, Fn);
  }

  void GenerateHookConstructor(CodeGenFunction &CGF,
                               ObjCHookDecl *HD) override {
    CGF.disableDebugInfo();

    llvm::Function *Fn = StartHookConstructor(CGF);

    llvm::CallInst *clazz = EmitGetClassRuntimeCall(
                                    CGF,
                                    HD->getClassInterface()->getNameAsString());

    llvm::CallInst *metaclass = EmitGetMetaclassRuntimeCall(CGF, clazz);

    for (ObjCContainerDecl::method_iterator M =
          HD->meth_begin(), MEnd = HD->meth_end(); M != MEnd; ++M) {

       ObjCMethodDecl *OMD = *M;

       llvm::Value *selector = CGM.getObjCRuntime().GetSelector(CGF, OMD);

       ObjCInterfaceDecl *CDecl = HD->getClassInterface();
       ObjCMethodDecl *mDecl = CDecl->lookupMethod(OMD->getSelector(),
                                                   OMD->isInstanceMethod());

       if (CDecl && mDecl) {
         if (mDecl->isNewMethod()) {
           EmitNewMethod(CGF,
                         OMD->isInstanceMethod() ? clazz : metaclass,
                         selector,
                         getMethodDefinition(OMD), OMD);
           continue;
         }
       }

       EmitMessageHookExRuntimeCall(
                              CGF,
                              OMD->isInstanceMethod() ? clazz : metaclass,
                              selector,
                              getMethodDefinition(OMD),
                              getOrigPointer(OMD));
    }

    CGF.FinishFunction(SourceLocation());

    HookConstructors.push_back(Fn);

    CGF.enableDebugInfo();
  }
  
  RValue GenerateOrigExpr(CodeGenFunction &CGF,
                          const ObjCOrigExpr *E, 
                          ReturnValueSlot Return) override {
    CGObjCRuntime &Runtime = CGM.getObjCRuntime();
    
    ObjCMethodDecl *OMD = E->getMethodDecl();
    
    CallArgList Args;
    
    Args.add(RValue::get(CGF.LoadObjCSelf()), CGF.getContext().getObjCIdType());
    Args.add(RValue::get(Runtime.GetSelector(CGF, OMD->getSelector())),
                         CGF.getContext().getObjCSelType());
    
    for (ObjCOrigExpr::const_arg_iterator I = E->arg_begin(), S = E->arg_end(); 
         I != S; ++I) {
      Args.add(CGF.EmitAnyExpr(*I), I->getType());
    }
    
    CGObjCRuntime::MessageSendInfo MSI =
                Runtime.getMessageSendInfo(OMD,
                                           OMD->getReturnType(), 
                                           Args);
    
    assert(isa<ObjCHookDecl>(OMD->getDeclContext()) &&
                                                      "@orig outside of @hook");
    
    // Load and call the original implementation  
    llvm::Value *Fn = CGF.Builder.CreateLoad(getOrigPointer(OMD));
    Fn = CGF.Builder.CreateBitCast(Fn, MSI.MessengerType);
    RValue rvalue = CGF.EmitCall(MSI.CallInfo, Fn, Return, Args);
    
    return rvalue;
}

};
}

CGObjCSRuntime* CodeGen::CreateMobileSubstrateObjCSRuntime(CodeGenModule &CGM) {
  return new CGObjCSMS(CGM);
}
