//*******************************************************
//
//           CodeGear Delphi Runtime Library
// Copyright(c) 2014-2023 Embarcadero Technologies, Inc.
//              All rights reserved
//
//*******************************************************

package com.embarcadero.services;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import com.embarcadero.rtl.NativeDispatchHelper;

public class <%ServiceName%>ProxyInterface implements InvocationHandler {
    long pointer;

    public Object CreateProxyClass(Class listenerClass, long pointer) throws ClassNotFoundException {
        this.pointer = pointer;
        return Proxy.newProxyInstance(listenerClass.getClassLoader(), new Class[] { listenerClass }, this);
    }

    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        Object obj = dispatchToNative2(method.getName(), NativeDispatchHelper.getMethodSignature(method.getReturnType(), method.getParameterTypes()), args, pointer);
        cleanNative(pointer);
        return obj;
    }

    public native Object dispatchToNative2(String methodName, String methodSig, Object[] args, long pointer);

    public native void cleanNative(long pointer);
}
