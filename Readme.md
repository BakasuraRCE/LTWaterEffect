**Programmer:** Leonel Togniolli (leonel@bestway.com.br)

**Description:** It simulates water effect, partially based on Roy Willense's

**Excellent tutorial, available (https://www.gamedev.net/articles/programming/graphics/the-water-effect-explained-r915)[here]**

**Delphinus-Support**

Also partially based on FLUID's demo (FLUiD - NeMeSiS production - only info I got).

## HOW TO USE IT:

Drop a TLTWaterEffect on a form, along with 2 TImages. Set one of them
to be the source image, one of them to be the destination image.
Load a bitmap (or draw) something in the source. Start the simulation.
You can call Disturb to animate a certain point or call StartRain/StopRain.
You might want to set the Form's DoubleBuffered to true, to avoid flickering.

## KNOWN BUGS:

There aren't many consistency checks so don't try anything that could break it :),
like a ScrImage smaller than the DstImage or starting it without setting
the images. I'll proably add the checks later.

## TO DO:

Optimize, optimize. It is still very slow, specially the ArcTan draw. It only
works for 32 bit images, maybe adding a way to chose the pixel format.