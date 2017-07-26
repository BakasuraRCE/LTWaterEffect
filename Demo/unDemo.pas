{ Unit : LTWaterEffect.Pas
  Programmer : Leonel Togniolli (leonel@bestway.com.br)
  Description : Demonstrate the usage of TLTWaterEffect
}
unit unDemo;

interface

uses
  Forms,Graphics, LTWaterEffect, StdCtrls, ExtCtrls, Controls, Classes;

type
  TForm1 = class(TForm)
    LTWaterEffect1: TLTWaterEffect;
    Image1: TImage;
    Image2: TImage;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var x,y:integer;
    P : PRGB32Array;
    BitMap : TBitMap;
begin
   DoubleBuffered := true;
   BitMap := TBitMap.Create;  // Draw Something nice...
   try
     with BitMap do
     begin
       height := Image1.height;
       width := Image1.width;
       PixelFormat := pf32bit;
       for y:=0 to height - 1 do
       begin
         P := ScanLine[y];
         x := 0;
         while x < width do
         begin
           P[x].R := x xor y;
           P[x].G := 0;
           P[x].B := 0;
           P[x].A := 0;
           inc(x);
         end;
       end;
     end;
     Image1.Canvas.Draw(0,0,BitMap);
   finally
     BitMap.Free;
   end;
   LTWaterEffect1.Start;  // Start the fun
end;

procedure TForm1.Image2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LTWaterEffect1.Disturb(X,Y,5000);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if LTWaterEffect1.Raining then
    LTWaterEffect1.StopRain
  else
    LTWaterEffect1.StartRain;
end;

end.
