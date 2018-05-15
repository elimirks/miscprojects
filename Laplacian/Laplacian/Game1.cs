using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;

using System;
using System.Numerics;

namespace Laplacian.Desktop
{
    public class Game1 : Game
    {
        int width, height;

        Texture2D canvas;
        Rectangle tracedSize;
        // Colors are stored as AABBGGRR
        UInt32[] texturePixels;

        Complex[] computePixels;
        // Magnitude
        double maxComputePixel;

        GraphicsDeviceManager graphics;
        SpriteBatch spriteBatch;

        int relaxationTimer = 0;

        public Game1()
        {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
        }

        protected override void Initialize()
        {
            width  = 400;
            height = 800;

            tracedSize = GraphicsDevice.PresentationParameters.Bounds;
            canvas = new Texture2D(GraphicsDevice,
                                   width, height, false, SurfaceFormat.Color);
            texturePixels = new UInt32[width * height];

            computePixels = new Complex[width * height];
            maxComputePixel = 1.0;

            // Top edge
            for (var i = 0; i < width; i++) {
                computePixels[i] = Complex.FromPolarCoordinates(1.0, 0);
            }

            // Bottom edge
            for (var i = 0; i < width; i++) {
                computePixels[i + (height - 1) * width] = Complex.FromPolarCoordinates(1.0, 1.25 * Math.PI);
            }

            // Left edge
            for (var i = 0; i < height; i++) {
                double phase = 2 * Math.PI * (double)i / height;
                computePixels[width * i] = Complex.FromPolarCoordinates(1.0, phase);
            }

            // Right edge
            for (var i = 0; i < height; i++) {
                computePixels[width * i + width - 1] = Complex.FromPolarCoordinates(1.0, 0.75 * Math.PI);
            }

            base.Initialize();
        }

        protected override void LoadContent()
        {
            // Create a new SpriteBatch, which can be used to draw textures.
            spriteBatch = new SpriteBatch(GraphicsDevice);

            graphics.PreferredBackBufferHeight = height;
            graphics.PreferredBackBufferWidth = width;
            graphics.ApplyChanges();
        }

        protected override void Update(GameTime gameTime)
        {
            if (Keyboard.GetState().IsKeyDown(Keys.Escape))
                Exit();

            // Start out with a course SOR, then go to a stable relaxation factor.
            if (relaxationTimer < 2000) {
                successiveRelaxation(1.95);
                relaxationTimer++;
            } else {
                successiveRelaxation(1.7);
            }

            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.Black);

            spriteBatch.Begin();
            computeRenderCanvas();
            spriteBatch.Draw(canvas, new Rectangle(0, 0, width, height), Color.White);
            spriteBatch.End();

            base.Draw(gameTime);
        }

        private void computeRenderCanvas() {
            for (var i = 0; i < computePixels.Length; i++) {
                const UInt32 alpha = 0xff000000;

                int r, g, b;

                double degPhase = computePixels[i].Phase * 180f / Math.PI;
                double magnitude = computePixels[i].Magnitude / maxComputePixel;

                HsvToRgb(degPhase, 1.0, magnitude, out r, out g, out b);
                // Colors are stored as AABBGGRR
                g *= 0x00000100;
                b *= 0x00010000;

                texturePixels[i] = alpha + (uint)(r + g + b);
            }
            canvas.SetData<UInt32>(texturePixels, 0, width * height);
        }

        // Faster algorithm than Gauss-Seidel, but could still use some improvement
        private void successiveRelaxation(double relaxationFactor) {
            // Never change the boundaries!
            for (int y = 1; y < height - 1; y++) {
                for (int x = 1; x < width - 1; x++) {
                    Complex surrounding = computePixels[width * (y-1) + x] +
                        computePixels[width * (y+1) + x] +
                        computePixels[width * y + (x-1)] +
                        computePixels[width * y + (x+1)];

                    computePixels[width * y + x] *= (1.0 - relaxationFactor);
                    computePixels[width * y + x] += (relaxationFactor / 4.0) * surrounding;
                }
            }
        }

        /// <summary>
        /// Convert HSV to RGB
        /// h is from 0-360
        /// s,v values are 0-1
        /// r,g,b values are 0-255
        /// Based upon http://ilab.usc.edu/wiki/index.php/HSV_And_H2SV_Color_Space#HSV_Transformation_C_.2F_C.2B.2B_Code_2
        /// </summary>
        private void HsvToRgb(double h, double S, double V, out int r, out int g, out int b) {
            // ######################################################################
            // T. Nathan Mundhenk
            // mundhenk@usc.edu
            // C/C++ Macro HSV to RGB

            double H = h;
            while (H < 0) { H += 360; };
            while (H >= 360) { H -= 360; };
            double R, G, B;
            if (V <= 0)
            { R = G = B = 0; }
            else if (S <= 0)
            {
                R = G = B = V;
            }
            else
            {
                double hf = H / 60.0;
                int i = (int)Math.Floor(hf);
                double f = hf - i;
                double pv = V * (1 - S);
                double qv = V * (1 - S * f);
                double tv = V * (1 - S * (1 - f));
                switch (i)
                {

                    // Red is the dominant color

                    case 0:
                        R = V;
                        G = tv;
                        B = pv;
                        break;

                        // Green is the dominant color

                    case 1:
                        R = qv;
                        G = V;
                        B = pv;
                        break;
                    case 2:
                        R = pv;
                        G = V;
                        B = tv;
                        break;

                        // Blue is the dominant color

                    case 3:
                        R = pv;
                        G = qv;
                        B = V;
                        break;
                    case 4:
                        R = tv;
                        G = pv;
                        B = V;
                        break;

                        // Red is the dominant color

                    case 5:
                        R = V;
                        G = pv;
                        B = qv;
                        break;

                        // Just in case we overshoot on our math by a little, we put these here. Since its a switch it won't slow us down at all to put these here.

                    case 6:
                        R = V;
                        G = tv;
                        B = pv;
                        break;
                    case -1:
                        R = V;
                        G = pv;
                        B = qv;
                        break;

                        // The color is not defined, we should throw an error.

                    default:
                        //LFATAL("i Value error in Pixel conversion, Value is %d", i);
                        R = G = B = V; // Just pretend its black/white
                        break;
                }
            }
            r = Clamp((int)(R * 255.0));
            g = Clamp((int)(G * 255.0));
            b = Clamp((int)(B * 255.0));
        }

        /// <summary>
        /// Clamp a value to 0-255
        /// </summary>
        private int Clamp(int i) {
            if (i < 0) return 0;
            if (i > 255) return 255;
            return i;
        }
    }
}
