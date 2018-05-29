{ Project to make SimpleIPC features of fpc inside a DLL so that any language
  can use simpleipc facilities for inter process communication

  Example: load fpc simpleipc dll into golang and communicate with fpc between
  two exe's. Then no DLL plugin is needed for a plugin system, separate exe's
  can be used.

  Todo: rename to dynsIpc.lpr

  Notes:
    -try also AdvancedIPC unit for more features not available in simpleipc
    -From fpc docs: "a SimpleIPC client in a desktop application cannot connect
     to a SimpleIPC server in a service application", so this will need to be
     noted if DLL is ever being used in a service
    -Exceptions - catch all common exceptions in DLL and return errors other
     ways (codes), do not let exception be handled in the exe as
     c/c++/delphi/other languages do not have a comptable exception mechanism
     or shared memory manager with fpc.

 Copyright Z505 Software

 License: bsd/mit }

library dynsimpleipc;

{$mode objfpc} {$H+}

uses
 {$ifdef unix}cthreads, {$endif}
  Classes,
  SimpleIPC,
  SysUtils,
  IpcParseMsg;

{$I libdeclare.inc}

type
  TIpc = class(TObject)
    Srv: TSimpleIPCServer;
    Threaded: boolean;
    DoStop: boolean;
    procedure MessageQueued(Sender: TObject);
    procedure PrintMessage;
  end;

var
  ipc: TIpc;

  // Server
  // -----------------------------------------------------------

  procedure DefaultStatusLn(s: string);
  begin
    writeln(':: ' + s);
  end;

var
  StatusLn: procedure(s: string) = @DefaultStatusLn;


  // write a status, "yes" if boolean true, "no" otherwise
  procedure StatusYesLn(b: boolean; s: string);
  begin
    if b then
      StatusLn(s + 'yes')
    else
      StatusLn(s + 'no');
  end;

  procedure TIpc.PrintMessage;
  var
    s: string;
  begin
    s := srv.StringMessage;
    //check message type, todo: how to put this in DLL?
    // if srv.msgtype = SOMETHING then
    // ...
    StatusLn('Received message: ' + S);
    DoStop := DoStop or (s = 'stop');
  end;

  // todo: callback mechanism for threads?
  // this function is currently not implemented
  procedure TIpc.MessageQueued(Sender: TObject);
  begin
    srv.ReadMessage;
    // PrintMessage;
  end;

{ Parameters:
  timeout: a good default to use is 10
  readopt: MSG_READ causes a readmessage to occur, MSG_NO_READ does not
  returns errors:
  0: success
  1: error when peeking for message
  2: exception when peeking for message
  3: ipc variable doesn't exist (is nil), server probably not created
  4: ipc server doesn't exist (is nil), server probably not created
  5: ipc server not active
}
  function sIpcPeekMsg(timeout: int32; readopt: int32): int32; cdecl;
  var
    peekrslt: boolean;
  begin
    if not assigned(ipc) then
      exit(3);
    if not assigned(ipc.srv) then
      exit(4);
    if not ipc.srv.active then
      exit(5);
    try
      case readopt of
        MSG_READ: peekrslt := ipc.srv.PeekMessage(timeout, True);
        MSG_NO_READ: peekrslt := ipc.srv.PeekMessage(timeout, False);
      end;
    except
      exit(2);
    end;
    if peekrslt then
      Result := 0
    else
      Result := 1;
  end;

(*
{
  // NOT implemented yet
  Try reading a message as a string
  Memory must be allocated for pchar buffer by caller.. make sure length of
  buffer is correct

  Returns
    1: error if reading message
    2: if msg available is not a string
    3: ipc variable does not exist (is nil), server probably not created
    4: ipc server does not exist (is nil), server probably not created
    5: ipc server not active
}
function sIpcReadMsgAsString(buf: pchar): int32; cdecl;
begin
  if not assigned(ipc) then exit(3);
  if not assigned(ipc.srv) then exit(4);
  if not ipc.srv.active exit(5);

  try
    ipc.srv.ReadMessage;
    // allocate memory for pchar by caller.. make sure length of buffer is correct
    if ipc.srv.MsgType = mtString then begin
      // TODO: copy into buffer here
      // is this function really needed? or is the callback mechanism good
      // enough since callback doesn't require memory allocation and this
      // function does?

      // copy( buf
    end else begin
      result := 2;
    end;
  except
    exit(1);
  end;
  result := 0;
end; exports sIpcReadMsgAsString;
*)


{ Checks for incoming message, and executes a callback function if there was a
  string message. Call this to check messages frequently in app. If the ipc
  server was threaded (in the DLL) then it checks for synchronization.
  Parameters
    peektime: amount of timeout peek call, good default to use is 10
    sleeptime: amount of time to sleep to rest cpu, 0 or < 1 means no sleep function will be called, good default to use is 10
  Returns errors:
  0: success
  1: problem converting message value(s) to an integer
  2: error when peeking for message
  3: callback parameter invalid (nil)
  4: this function does not work in OPT_THREADED mode
  5: ipc variable is not created (it is nil)
  6: ipc server is not created (it is nil)
}
  function sIpcExecOnMsg(peektime: int32; sleeptime: int32;
    cbString: TCallbackString; cbInt32: TCallbackInt32; cbXY: TCallbackXY;
    cbInts: TCallbackInts; cbIntStr: TCallbackIntStr): int32; cdecl;

    procedure DoSleep;
    begin
      if sleeptime > 0 then
        sleep(sleeptime);
    end;

  var
    x, y, x1, x2, x3, x4: integer;
    recvdstring: string;
    recvdint32: int32;
    success: boolean;
    tmpint: integer;
    tmpstr: string;
  begin
    if not assigned(ipc) then
      exit(6);
    if not assigned(ipc.srv) then
      exit(5);
    if ipc.Threaded then
      exit(4);

    try
      if ipc.srv.PeekMessage(peektime, True) then
      begin
        recvdstring := ipc.srv.StringMessage;
        case ipc.srv.MsgType of
          mtString:
          begin
            if cbString <> nil then
            begin
              cbString(PChar(recvdstring));
            end
            else
            begin
              exit(3);
            end;
          end;

          mtInt32:
          begin
            success := TryStrToInt(recvdstring, recvdint32);
            if not success then
              exit(1);
            if cbInt32 <> nil then
            begin
              cbInt32(recvdint32);
            end
            else
            begin
              exit(3);
            end;
          end;

          mtXY:
          begin
            success := parse2Ints(recvdstring, x, y);
            if not success then
              exit(1);
            if cbXY <> nil then
            begin
              cbXY(x, y);
            end
            else
            begin
              exit(3);
            end;
          end;

          mtInts:
          begin
            success := parseInts(recvdstring, x1, x2, x3, x4);
            if not success then
              exit(1);
            if cbInts <> nil then
            begin
              cbInts(x1, x2, x3, x4);
            end
            else
            begin
              exit(3);
            end;
          end;

          mtIntStr:
          begin
            success := ParseIntAndStr(recvdstring, tmpint, tmpstr);
            if not success then
              exit(1);
            if cbIntStr <> nil then
            begin
              cbIntStr(tmpint, PChar(tmpstr));
            end
            else
            begin
              exit(3);
            end;
          end;
        end; {case}
      end
      else
      begin
        DoSleep;
      end;
    except
      exit(2);
    end;
    Result := 0;
  end;

  function sIpcExecOnString(peektime: int32; sleeptime: int32;
    cb: TCallbackString): int32; cdecl;
  begin
    Result := sIpcExecOnMsg(peektime, sleeptime, cb, nil, nil, nil, nil);
  end;

  function sIpcExecOnInt32(peektime: int32; sleeptime: int32;
    cb: TCallbackInt32): int32; cdecl;
  begin
    Result := sIpcExecOnMsg(peektime, sleeptime, nil, cb, nil, nil, nil);
  end;

  function sIpcExecOnXY(peektime: int32; sleeptime: int32; cb: TCallbackXY): int32; cdecl;
  begin
    Result := sIpcExecOnMsg(peektime, sleeptime, nil, nil, cb, nil, nil);
  end;

  function sIpcExecOnInts(peektime: int32; sleeptime: int32;
    cb: TCallbackInts): int32; cdecl;
  begin
    Result := sIpcExecOnMsg(peektime, sleeptime, nil, nil, nil, cb, nil);
  end;

  function sIpcExecOnIntStr(peektime: int32; sleeptime: int32;
    cb: TCallbackIntStr): int32; cdecl;
  begin
    Result := sIpcExecOnMsg(peektime, sleeptime, nil, nil, nil, nil, cb);
  end;

{ returns errors:
   0: no errors
   1: ipc variable already created, can't create again until freed first
   2: ipc server already created, can't create again until freed first }
  function sIpcCreateServer: int32; cdecl;
  begin
    if assigned(ipc) then
    begin
      if assigned(ipc.srv) then
        exit(2);
      exit(1);
    end;

    ipc := TIpc.Create;
    ipc.srv := TSimpleIPCServer.Create(nil);
    Result := 0;
  end;

{ returns
  0: success
  1: error stopping the server
  2: ipc variable not assigned (is nil), can't free variable that doesn't exist
  3: ipc server variable not assigned, trying to free server that doesn't exist }
  function sIpcFreeServer: int32; cdecl;

    procedure cleanup;
    begin
      ipc.srv.Free;
      ipc.srv := nil;
      ipc.Free;
      ipc := nil;
    end;

  begin
    if not assigned(ipc) then
      exit(2);
    if not assigned(ipc.srv) then
      exit(3);
    try
      // StatusYesLn(ipc.srv.Active, 'Server active: ');
      ipc.srv.StopServer;
    except
      cleanup;
      exit(1);
    end;
    cleanup;
    Result := 0;
  end;

{ returns errors:
  0: success
  1: cannot create threaded server on this OS/platform
  2: error running server
  3: IPC variable not created (is nil)
  4: ipc server variable not created (is nil) }
  function sIpcStartServer(servID: PChar; threaded: int32): int32; cdecl;
  begin
    if not assigned(ipc) then
      exit(3);
    if not assigned(ipc.srv) then
      exit(4);
    try
      ipc.srv.ServerID := servID;
      // StatusLn('IPC server created');
      ipc.srv.ServerID := servID;
      ipc.srv.global := True;
      case threaded of
        OPT_THREAD: ipc.Threaded := True;
        OPT_NO_THREAD: ipc.Threaded := False;
        else
          ipc.Threaded := False;
      end;

      if ipc.Threaded then
      begin
     {$IFDEF windows}
        // simpleipc threaded needs to fixed on windows, see fpc bug report
        exit(1);
     {$ELSE}
        // StatusLn('using threaded ipc server');
        ipc.srv.OnMessageQueued := @ipc.MessageQueued;
     {$ENDIF}
      end;
      ipc.srv.StartServer(ipc.threaded);
    except
      exit(2);
    end;
    Result := 0;
  end;

  { simple test with default test server ID, returns error if any }
  function sIpcStartServerTest: int32; cdecl;
  begin
    Result := sIpcStartServer(TEST_SERVER_ID, OPT_NO_THREAD);
  end;

  // Client
  // -----------------------------------------------------------

var
  IpcCli: TSimpleIPCClient;

{ returns errors:
   0: success
   1: ipc client already created, can't create until freed first }
  function sIpcCreateClient(): int32; cdecl;
  begin
    if assigned(IpcCli) then
      exit(1);
    IpcCli := TSimpleIPCClient.Create(nil);
    Result := 0;
  end;

{ returns errors:
    0: success
    1: ipc client not assigned (is nil), can't free variable that doesn't exist }
  function sIpcFreeClient: int32; cdecl;
  begin
    if not assigned(IpcCli) then
      exit(1);
    IpcCli.Free;
    IpcCli := nil;
    Result := 0;
  end;

{ returns errors:
    0: success
    1: IpcCli not assigned (is nil), not created
    2: error connecting to server }
  function sIpcStartClient(servID: PChar): int32; cdecl;
  begin
    if not assigned(IpcCli) then
      exit(1);

    try
      IpcCli.ServerID := servID;
      IpcCli.Connect;
    except
      exit(2);
    end;
    Result := 0;
  end;

{ General function to send any msg type
  Returns errors:
    0: success
    1: ipc client not assigned (is nil), it is not created
    2: ipc client not active
    3: ipc server is not running }
  function SendMsg(msgType: int32; x1: int32; x2: int32; x3: int32;
    x4: int32; s: PChar): int32;
  begin
    if not assigned(IpcCli) then
      exit(1);
    if not IpcCli.Active then
      exit(2);
    if not IpcCli.ServerRunning then
      exit(3);

    case msgType of
      mtUnknown: { not implemented };
      mtString: IpcCli.SendStringMessage(msgType, s);
      mtInt32: IpcCli.SendStringMessage(msgType, IntToStr(x1));
      mtXY: IpcCli.SendStringMessage(msgType, IntToStr(x1) + ';' + IntToStr(x2));
      mtInts: IpcCli.SendStringMessage(msgType,
          IntToStr(x1) + ';' + IntToStr(x2) + ';' + IntToStr(x3) + ';' + IntToStr(x4));
      mtIntStr: IpcCli.SendStringMessage(msgType, IntToStr(x1) + ';' + s);
    end;
    Result := 0;
  end;

  function sIpcSendStringMsg(s: PChar): int32; cdecl;
  begin
    Result := SendMsg(mtString, 0, 0, 0, 0, s);
  end;

  function sIpcSendXYMsg(x: int32; y: int32): int32; cdecl;
  begin
    Result := SendMsg(mtXY, x, y, 0, 0, nil);
  end;

  function sIpcSendInt32Msg(i: int32): int32; cdecl;
  begin
    Result := SendMsg(mtInt32, i, 0, 0, 0, nil);
  end;

  function sIpcSendIntStrMsg(i: int32; s: PChar): int32; cdecl;
  begin
    Result := SendMsg(mtIntStr, i, 0, 0, 0, s);
  end;

  function sIpcSendIntsMsg(x1: int32; x2: int32; x3: int32; x4: int32): int32; cdecl;
  begin
    Result := SendMsg(mtInts, x1, x2, x3, x4, nil);
  end;



exports
  sIpcPeekMsg
{$IFDEF Darwin}
  Name '_sIpcPeekMsg'
{$ENDIF}
  ,
  sIpcExecOnMsg
{$IFDEF Darwin}
  Name '_sIpcExecOnMsg'
{$ENDIF}
  ,
  sIpcExecOnString
{$IFDEF Darwin}
  Name '_sIpcExecOnString'
{$ENDIF}
  ,
  sIpcExecOnInt32
{$IFDEF Darwin}
  Name '_sIpcExecOnInt32'
{$ENDIF}
  ,
  sIpcExecOnXY
{$IFDEF Darwin}
  Name '_sIpcExecOnXY'
{$ENDIF}
  ,
  sIpcExecOnInts
{$IFDEF Darwin}
  Name '_sIpcExecOnInts'
{$ENDIF}
  ,
  sIpcExecOnIntStr
{$IFDEF Darwin}
  Name '_sIpcExecOnIntStr'
{$ENDIF}
  ,
  sIpcCreateServer
{$IFDEF Darwin}
  Name '_sIpcCreateServer'
{$ENDIF}
  ,
  sIpcFreeServer
{$IFDEF Darwin}
  Name '_sIpcFreeServer'
{$ENDIF}
  ,
  sIpcStartServer
{$IFDEF Darwin}
  Name '_sIpcStartServer'
{$ENDIF}
  ,
  sIpcStartServerTest
{$IFDEF Darwin}
  Name '_sIpcStartServerTest'
{$ENDIF}
  ,
  sIpcCreateClient
{$IFDEF Darwin}
  Name '_sIpcCreateClient'
{$ENDIF}
  ,
  sIpcFreeClient
{$IFDEF Darwin}
  Name '_sIpcFreeClient'
{$ENDIF}
  ,
  sIpcStartClient
{$IFDEF Darwin}
  Name '_sIpcStartClient'
{$ENDIF}
  ,
  sIpcSendStringMsg
{$IFDEF Darwin}
  Name '_sIpcSendStringMsg'
{$ENDIF}
  ,
  sIpcSendXYMsg
{$IFDEF Darwin}
  Name '_sIpcSendXYMsg'
{$ENDIF}
  ,
  sIpcSendInt32Msg
{$IFDEF Darwin}
  Name '_sIpcSendInt32Msg'
{$ENDIF}
  ,
  sIpcSendIntStrMsg
{$IFDEF Darwin}
  Name '_sIpcSendIntStrMsg'
{$ENDIF}
  ,
  sIpcSendIntsMsg
{$IFDEF Darwin}
  Name '_sIpcSendIntsMsg'
{$ENDIF}
  ;

end.
