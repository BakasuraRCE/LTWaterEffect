{ Unit : LTWaterEffect.Pas
  Programmer : Leonel Togniolli (leonel@bestway.com.br)
  Description : It simulates water effect, partially based on Roy Willense's
  excellent tutorial, available at:
  http://www.gamedev.net/reference/articles/article915.asp
  Also partially based on FLUID's demo (FLUiD - NeMeSiS production - only
  info I got).

  HOW TO USE IT:

  Drop a TLTWaterEffect on a form, along with 2 TImages. Set one of them
  to be the source image, one of them to be the destination image.
  Load a bitmap (or draw) something in the source. Start the simulation.
  You can call Disturb to animate a certain point or call StartRain/StopRain.
  You might want to set the Form's DoubleBuffered to true, to avoid flickering.

  KNOWN BUGS:

  There aren't many consistency checks so don't try anything that could break it :),
  like a ScrImage smaller than the DstImage or starting it without setting
  the images. I'll proably add the checks later.

  TO DO:

  Optimize, optimize. It is still very slow, specially the ArcTan draw. It only
  works for 32 bit images, maybe adding a way to chose the pixel format.

  LICENSE:

  There's no actual license, you are free to do what you like with it, just
  give me some credit. I would also like to ask if you correct any bugs,
  enhance or change it in any way send me the modifications. Finally, if you
  like it, drop me a line telling me that you liked. If you don't, drop me a
  line telling why. I hope someone learns as much I did writing it.

}

unit LTWaterEffect;

interface

uses
  SysUtils, Classes, ExtCtrls;

type
  TRGB32 = packed record
    B, G, R, A: byte;
  end;

  PRGB32Array = ^TRGB32Array;
  PRGB32 = ^TRGB32;
  TRGB32Array = packed array [0 .. maxint div sizeof(TRGB32) - 1] of TRGB32;
  TWaveAlgorithm = (waSinTan, waCalc);
  TIntegerArray = array of Integer;

  TLTWaterEffect = class(TComponent)
  private
    FDstImage: TImage;
    FSrcImage: TImage;
    FTimer: TTimer;
    FHeight: Integer;
    FWidth: Integer;
    FReflectionIndex: Integer;
    FDensity: Integer;
    FDrawing: Boolean;
    FPage: byte;
    FHeightMap: array [0 .. 1] of TIntegerArray;
    FAlgorithm: TWaveAlgorithm;
    FTimerInterval: Integer;
    FRefresh: Boolean;
    FCurrentPage: ^TIntegerArray;
    FNewPage: ^TIntegerArray;
    FActive: Boolean;
    FDst: PRGB32;
    FPoint: PRGB32;
    FScanSize: LongInt;
    FRainAmount: Integer;
    FRaining: Boolean;
    FRainTimerInterval: Integer;
    FRainTimer: TTimer;
    procedure SetDstImage(const Value: TImage);
    procedure SetSrcImage(const Value: TImage);
    procedure SetReflecionIndex(const Value: Integer);
    procedure SetDensity(const Value: Integer);
    procedure SetAlgorithm(const Value: TWaveAlgorithm);
    procedure SetTimerInterval(const Value: Integer);
    procedure SetRefresh(const Value: Boolean);
    procedure SetRainAmount(const Value: Integer);
    procedure SetRainTimerInterval(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawSinTan;
    procedure DrawCalc;
    procedure Calculate;
    procedure DoTimer(Sender: TObject);
    procedure DoRainTimer(Sender: TObject);
  public
    procedure Disturb(X, Y, Amount: Integer);
    procedure Start;
    procedure Stop;
    procedure StartRain;
    procedure StopRain;
    property Active: Boolean read FActive;
    property Raining: Boolean read FRaining;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SrcImage: TImage read FSrcImage write SetSrcImage;
    property DstImage: TImage read FDstImage write SetDstImage;
    property ReflectionIndex: Integer read FReflectionIndex write SetReflecionIndex default 2;
    property Density: Integer read FDensity write SetDensity default 4;
    property Algorithm: TWaveAlgorithm read FAlgorithm write SetAlgorithm default waSinTan;
    property TimerInterval: Integer read FTimerInterval write SetTimerInterval default 5;
    property Refresh: Boolean read FRefresh write SetRefresh default true;
    property RainAmount: Integer read FRainAmount write SetRainAmount default 1000;
    property RainTimerInterval: Integer read FRainTimerInterval write SetRainTimerInterval default 150;
  end;

procedure Register;

implementation

uses Math, Dialogs, Graphics;

const
  cSizeOfRGB = 4;

procedure Register;
begin
  RegisterComponents('LT', [TLTWaterEffect]);
end;

{ TLTWaterEffect }

procedure TLTWaterEffect.Calculate;
var
  NewVal, Count, I, J: Integer;
begin
  Count := FHeight + 1;
  for I := 1 to FWidth - 1 do
  begin
    for J := 1 to FHeight - 2 do
    begin
      NewVal := ((FCurrentPage^[Count - FHeight] + FCurrentPage^[Count - 1] + FCurrentPage^[Count + 1] +
            FCurrentPage^[Count + FHeight]) shr 1) - FNewPage^[Count];
      if NewVal < 0 then
        FNewPage^[Count] := 0 // Why can't I drop below 0?
      else
      begin
        FNewPage^[Count] := NewVal - (NewVal shr Density);
      end;
      inc(Count);
    end;
    inc(Count, 2);
  end;
end;

constructor TLTWaterEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.OnTimer := DoTimer;
  FTimerInterval := 5;
  FTimer.Interval := 5;
  FReflectionIndex := 2;
  FDensity := 4;
  FPage := 0;
  FRainAmount := 1000;
  FRefresh := true;
  FAlgorithm := waSinTan;
  FRainTimer := TTimer.Create(nil);
  FRainTimer.Enabled := false;
  FRainTimer.OnTimer := DoRainTimer;
  FRainTimerInterval := 150;
  FRainTimer.Interval := 150;
end;

destructor TLTWaterEffect.Destroy;
begin
  FTimer.Free;
  FRainTimer.Free;
  inherited;
end;

procedure TLTWaterEffect.Disturb(X, Y, Amount: Integer);
begin
  if FActive then
    if (X > 1) and (Y > 1) and (X < FWidth - 1) and (Y < FHeight - 1) then
      FHeightMap[FPage][(FWidth - X) * FHeight + Y] := Amount;
end;

procedure TLTWaterEffect.DoRainTimer(Sender: TObject);
begin
  if FActive then
    FHeightMap[FPage][RandomRange(1, FWidth - 1) * FHeight + RandomRange(1, FHeight - 1)] := FRainAmount;
end;

procedure TLTWaterEffect.DoTimer(Sender: TObject);
begin
  if FDrawing then
    exit;
  FDrawing := true;
  try
    FCurrentPage := @FHeightMap[FPage]; // Used pointers hoping to get some
    FNewPage := @FHeightMap[1 - FPage]; // speed up, avoiding accessing the array.
    if FAlgorithm = waSinTan then
      DrawSinTan
    else
      DrawCalc;
    Calculate;
    FPage := 1 - FPage;
  finally
    if FRefresh then
      DstImage.Refresh;
    FDrawing := false;
  end;
end;

procedure TLTWaterEffect.DrawCalc; // From FLUID C/ASM Demo
var
  C, O, oX, oY, X1, Y1, X, Y: Integer;
  P: PRGB32;
  Dst: PRGB32;
begin
  Dst := FDst;
  for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      O := FHeight * X + Y;
      oX := FCurrentPage^[O] - FCurrentPage^[O + FHeight];
      oY := FCurrentPage^[O] - FCurrentPage^[O + 1];
      C := 128 - oX;
      if (C > 0) then
      begin
        if (C > 255) then
          C := 255;
        X1 := (oX shr 3) + X;
        Y1 := (oY shr 3) + Y;
        if (X1 < 0) or (Y1 < 0) or (X1 > FWidth - 1) or (Y1 > FHeight - 1) then
        begin
          dec(Dst);
          continue;
        end;
        P := PRGB32(LongInt(FPoint) - Y1 * FScanSize + X1 * cSizeOfRGB);
        Dst^.R := (P^.R * C) shr 7;
        Dst^.G := (P^.G * C) shr 7;
        Dst^.B := (P^.B * C) shr 7;
      end
      else
      begin
        Dst^.R := 0;
        Dst^.G := 0;
        Dst^.B := 0;
      end;
      dec(Dst);
    end;
  end;
end;

procedure TLTWaterEffect.DrawSinTan; // From Roy Willemse Tutorial. Needs optimizations back. :)
var
  XDisplace, YDisplace,
    O, oX, oY, X1, Y1, X, Y: Integer;
  yAngle, yRefraction,
    xAngle, XRefraction: Single;
  Dst: PRGB32;

begin
  Dst := FDst;
  for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      O := FHeight * X + Y;
      oX := FCurrentPage^[O] - FCurrentPage^[O + FHeight];
      oY := FCurrentPage^[O] - FCurrentPage^[O + 1];

      xAngle := ArcTan(oX);
      XRefraction := ArcSin(Sin(xAngle) / FReflectionIndex);
      XDisplace := Trunc(Tan(XRefraction) * oX);

      yAngle := ArcTan(oY);
      yRefraction := ArcSin(Sin(yAngle) / FReflectionIndex);
      YDisplace := Trunc(Tan(yRefraction) * oY);
      if oX < 0 then
      begin
        if oY < 0 then
        begin
          X1 := X - XDisplace;
          Y1 := Y - YDisplace;
        end
        else
        begin
          X1 := X - XDisplace;
          Y1 := Y + YDisplace;
        end;
      end
      else
      begin
        if oY < 0 then
        begin
          X1 := X + XDisplace;
          Y1 := Y - YDisplace;
        end
        else
        begin
          X1 := X + XDisplace;
          Y1 := Y + YDisplace;
        end;
      end;
      if (X1 < 0) or (Y1 < 0) or (X1 > FWidth - 1) or (Y1 > FHeight - 1) then
      begin
        dec(Dst);
        continue;
      end;
      Dst^ := PRGB32(LongInt(FPoint) - (Y1 * FScanSize + X1 * cSizeOfRGB))^;
      dec(Dst);
    end;
  end;
end;

procedure TLTWaterEffect.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = SrcImage then
      SrcImage := nil
    else if AComponent = DstImage then
      DstImage := nil;
end;

procedure TLTWaterEffect.SetAlgorithm(const Value: TWaveAlgorithm);
begin
  FAlgorithm := Value;
end;

procedure TLTWaterEffect.SetDensity(const Value: Integer);
begin
  FDensity := Value;
end;

procedure TLTWaterEffect.SetDstImage(const Value: TImage);
begin
  FDstImage := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TLTWaterEffect.SetRainAmount(const Value: Integer);
begin
  FRainAmount := Value;
end;

procedure TLTWaterEffect.SetRainTimerInterval(const Value: Integer);
begin
  if FRainTimerInterval <> Value then
  begin
    FRainTimerInterval := Value;
    FRainTimer.Interval := Value;
  end;
end;

procedure TLTWaterEffect.SetReflecionIndex(const Value: Integer);
begin
  FReflectionIndex := Value;
end;

procedure TLTWaterEffect.SetRefresh(const Value: Boolean);
begin
  FRefresh := Value;
end;

procedure TLTWaterEffect.SetSrcImage(const Value: TImage);
begin
  FSrcImage := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TLTWaterEffect.SetTimerInterval(const Value: Integer);
begin
  if FTimerInterval <> Value then
  begin
    FTimerInterval := Value;
    FTimer.Interval := Value;
  end;
end;

procedure TLTWaterEffect.Start;
var
  I, J: Integer;
begin
  FDrawing := false;
  FHeight := DstImage.Height;
  FWidth := DstImage.Width;
  SrcImage.Picture.Bitmap.PixelFormat := pf32bit;
  DstImage.Picture.Bitmap.Width := FWidth;
  DstImage.Picture.Bitmap.Height := FHeight;
  DstImage.Picture.Bitmap.PixelFormat := pf32bit;
  for I := 0 to 1 do
  begin
    try
      SetLength(FHeightMap[I], (FWidth + 1) * (FHeight + 1));
    except
      on EOutOfMemory do
        MessageDlg('Not Enough Memory!', mterror, [mbok], 0);
      else
        raise;
    end;
    for J := 0 to (FWidth + 1) * (FHeight + 1) - 1 do
      FHeightMap[I][J] := 0;
  end;
  for I := 1 to FHeight - 1 do
    FDst := DstImage.Picture.Bitmap.ScanLine[I];
  FDst := DstImage.Picture.Bitmap.ScanLine[0];
  FPoint := SrcImage.Picture.Bitmap.ScanLine[0];
  FScanSize := (LongInt(FPoint) - LongInt(SrcImage.Picture.Bitmap.ScanLine[1]));
  FDst := pointer(LongInt(FDst) + (FScanSize - cSizeOfRGB)); // These point to the last pixel
  FPoint := pointer(LongInt(FPoint) + (FScanSize - cSizeOfRGB)); // in the image. We work our way up.
  FActive := true;
  FTimer.Enabled := true;
end;

procedure TLTWaterEffect.StartRain;
begin
  if FActive and not FRaining then
  begin
    Randomize;
    FRaining := true;
    FRainTimer.Enabled := true;
  end;
end;

procedure TLTWaterEffect.Stop;
var
  I: Integer;
begin
  FTimer.Enabled := false;
  FActive := false;
  while FDrawing do { loop };
  for I := 0 to 1 do
    SetLength(FHeightMap[I], 0);
end;

procedure TLTWaterEffect.StopRain;
begin
  if FRaining then
  begin
    FRainTimer.Enabled := false;
    FRaining := false;
  end;
end;

end.
