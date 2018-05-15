using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;

using System;

namespace Laplacian.Desktop
{
    public class Game1 : Game
    {
        int width, height;

        Texture2D canvas;
        Rectangle tracedSize;
        // Colors are stored as AABBGGRR
        UInt32[] texturePixels;

        double[] computePixels;
        // Magnitude
        double maxComputePixel;

        GraphicsDeviceManager graphics;
        SpriteBatch spriteBatch;

        public Game1()
        {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
        }

        protected override void Initialize()
        {
            height = 800;
            width  = 400;

            tracedSize = GraphicsDevice.PresentationParameters.Bounds;
            canvas = new Texture2D(GraphicsDevice,
                                   width, height, false, SurfaceFormat.Color);
            texturePixels = new UInt32[width * height];

            computePixels = new double[width * height];

            for (var i = 0; i < width; i++) {
                computePixels[i] = 1.0;
            }

            for (var i = 0; i < height; i++) {
                computePixels[width * i] = - Math.Sin(4 * 2 * Math.PI * (double)i / height);
            }

            maxComputePixel = 1.0;

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

            successiveRelaxation();

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

                double value = computePixels[i];
                UInt32 magnitude = (UInt32)(Math.Min(1.0, Math.Abs(value) / maxComputePixel) * 256f);

                UInt32 colorIdentity = (UInt32)((value < 0f) ? 0x00000001 : 0x00000100);
                texturePixels[i] = alpha + colorIdentity * magnitude;
            }
            canvas.SetData<UInt32>(texturePixels, 0, width * height);
        }

        // Faster algorithm than Gauss-Seidel, but could still use some improvement
        private void successiveRelaxation() {
            // SR parameter
            const double s = 1.7;

            for (int y = 1; y < height - 1; y++) {
                for (int x = 1; x < width - 1; x++) {
                    computePixels[width * y + x] = computePixels[width * y + x] * (1.0-s) + (s / 4.0) * (
                        computePixels[width * (y-1) + x] +
                        computePixels[width * (y+1) + x] +
                        computePixels[width * y + (x-1)] +
                        computePixels[width * y + (x+1)]);
                }
            }
        }
    }
}
