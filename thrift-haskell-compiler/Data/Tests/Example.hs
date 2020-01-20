{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Tests.Example where
import qualified Thrift.Type as Thrift
import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe

data SampleStruct = SampleStruct{sampleStructKey :: Thrift.Int32,
                                 sampleStructValue :: Thrift.Text}
                  deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                            Thrift.Hashable)

instance Thrift.Thrift SampleStruct where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let sampleStructKey = Thrift.defaultValue
                sampleStructValue = Thrift.defaultValue
              in SampleStruct{..}
        toTValue SampleStruct{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue sampleStructKey),
                  Just (2, Thrift.toTValue sampleStructValue)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                sampleStructKey = Thrift.lookupDefault Thrift.defaultValue 1 m
                sampleStructValue = Thrift.lookupDefault Thrift.defaultValue 2 m
              in SampleStruct{..}
        fromTValue _ = error "bad struct value"

data SampleServiceHelloThereReq = SampleServiceHelloThereReq{sampleServiceHelloThereReqHelloString
                                                             :: Thrift.Text}
                                deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                          Thrift.Hashable)

instance Thrift.Thrift SampleServiceHelloThereReq where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let sampleServiceHelloThereReqHelloString = Thrift.defaultValue
              in SampleServiceHelloThereReq{..}
        toTValue SampleServiceHelloThereReq{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue sampleServiceHelloThereReqHelloString)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                sampleServiceHelloThereReqHelloString
                  = Thrift.lookupDefault Thrift.defaultValue 1 m
              in SampleServiceHelloThereReq{..}
        fromTValue _ = error "bad struct value"

data SampleServiceHelloThereRes = SampleServiceHelloThereRes Thrift.Text
                                deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                          Thrift.Hashable)

instance Thrift.Thrift SampleServiceHelloThereRes where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue = SampleServiceHelloThereRes Thrift.defaultValue
        toTValue (SampleServiceHelloThereRes x)
          = Thrift.TStruct [(0, Thrift.toTValue x)]
        fromTValue (Thrift.TStruct m)
          | Just x <- Thrift.lookup 0 m' =
            SampleServiceHelloThereRes (Thrift.fromTValue x)
          | otherwise = error "bad union struct"
          where m' = Thrift.mkIntMap m
        fromTValue _ = error "bad union struct value"

sampleServiceHelloThere ::
                        Thrift.Protocol ->
                          Thrift.Transport ->
                            SampleServiceHelloThereReq -> IO SampleServiceHelloThereRes
sampleServiceHelloThere = Thrift.request "HelloThere" False

type SampleServiceServerDoSomethingReq = ()

type SampleServiceServerDoSomethingRes = ()

sampleServiceServerDoSomething ::
                               Thrift.Protocol ->
                                 Thrift.Transport ->
                                   SampleServiceServerDoSomethingReq ->
                                     IO SampleServiceServerDoSomethingRes
sampleServiceServerDoSomething
  = Thrift.request "ServerDoSomething" False

data SampleServiceClientSideListenPortReq = SampleServiceClientSideListenPortReq{sampleServiceClientSideListenPortReqPort
                                                                                 :: Thrift.Int16}
                                          deriving (Eq, Show, Thrift.Data, Thrift.Typeable,
                                                    Thrift.Generic, Thrift.Hashable)

instance Thrift.Thrift SampleServiceClientSideListenPortReq where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let sampleServiceClientSideListenPortReqPort
                  = Thrift.defaultValue
              in SampleServiceClientSideListenPortReq{..}
        toTValue SampleServiceClientSideListenPortReq{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just
                    (1, Thrift.toTValue sampleServiceClientSideListenPortReqPort)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                sampleServiceClientSideListenPortReqPort
                  = Thrift.lookupDefault Thrift.defaultValue 1 m
              in SampleServiceClientSideListenPortReq{..}
        fromTValue _ = error "bad struct value"

type SampleServiceClientSideListenPortRes = ()

sampleServiceClientSideListenPort ::
                                  Thrift.Protocol ->
                                    Thrift.Transport ->
                                      SampleServiceClientSideListenPortReq ->
                                        IO SampleServiceClientSideListenPortRes
sampleServiceClientSideListenPort
  = Thrift.request "ClientSideListenPort" False

data SampleServiceClientSidePipeNameReq = SampleServiceClientSidePipeNameReq{sampleServiceClientSidePipeNameReqName
                                                                             :: Thrift.Text}
                                        deriving (Eq, Show, Thrift.Data, Thrift.Typeable,
                                                  Thrift.Generic, Thrift.Hashable)

instance Thrift.Thrift SampleServiceClientSidePipeNameReq where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let sampleServiceClientSidePipeNameReqName = Thrift.defaultValue
              in SampleServiceClientSidePipeNameReq{..}
        toTValue SampleServiceClientSidePipeNameReq{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue sampleServiceClientSidePipeNameReqName)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                sampleServiceClientSidePipeNameReqName
                  = Thrift.lookupDefault Thrift.defaultValue 1 m
              in SampleServiceClientSidePipeNameReq{..}
        fromTValue _ = error "bad struct value"

type SampleServiceClientSidePipeNameRes = ()

sampleServiceClientSidePipeName ::
                                Thrift.Protocol ->
                                  Thrift.Transport ->
                                    SampleServiceClientSidePipeNameReq ->
                                      IO SampleServiceClientSidePipeNameRes
sampleServiceClientSidePipeName
  = Thrift.request "ClientSidePipeName" False

data SampleServiceHandler = SampleServiceHandler{sampleServiceHelloThereHandler
                                                 ::
                                                 SampleServiceHelloThereReq ->
                                                   IO SampleServiceHelloThereRes,
                                                 sampleServiceServerDoSomethingHandler ::
                                                 SampleServiceServerDoSomethingReq ->
                                                   IO SampleServiceServerDoSomethingRes,
                                                 sampleServiceClientSideListenPortHandler ::
                                                 SampleServiceClientSideListenPortReq ->
                                                   IO SampleServiceClientSideListenPortRes,
                                                 sampleServiceClientSidePipeNameHandler ::
                                                 SampleServiceClientSidePipeNameReq ->
                                                   IO SampleServiceClientSidePipeNameRes}

type SampleCallbackPingclientReq = ()

type SampleCallbackPingclientRes = ()

sampleCallbackPingclient ::
                         Thrift.Protocol ->
                           Thrift.Transport ->
                             SampleCallbackPingclientReq -> IO SampleCallbackPingclientRes
sampleCallbackPingclient = Thrift.request "pingclient" False

data SampleCallbackHandler = SampleCallbackHandler{sampleCallbackPingclientHandler
                                                   ::
                                                   SampleCallbackPingclientReq ->
                                                     IO SampleCallbackPingclientRes}