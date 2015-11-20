//===---------- CGObjCS.cpp - Emit LLVM Code for Objective-CS -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit Objective-CS code as LLVM code.
//
//===----------------------------------------------------------------------===//

#include "CGObjCSRuntime.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"

using namespace clang;
using namespace CodeGen;

void CodeGenFunction::GenerateObjCSMethodHook(const ObjCMethodDecl *OMD,
                                              const ObjCHookDecl *HD) {
  CGM.getObjCSRuntime().GenerateMethodHook(*this, OMD, HD);
}

void CodeGenFunction::GenerateObjCSHookConstructor(ObjCHookDecl *HD) {
  CGM.getObjCSRuntime().GenerateHookConstructor(*this, HD);
}

RValue CodeGenFunction::EmitObjCSOrigExpr(const ObjCOrigExpr *E,
                                          ReturnValueSlot Return) {
  return CGM.getObjCSRuntime().GenerateOrigExpr(*this, E, Return);
}


CGObjCSRuntime::~CGObjCSRuntime() { }
