<script lang="ts">
    const padding = 5.0;
    const charWidth = 40.0;
    const charHeight = 80.0;
    // Bitmasks of 16 bits each
    export let chars: number[] = [];
    let charCount = chars.length;
</script>
<svg xmlns="http://www.w3.org/2000/svg" width={2 * padding + charCount * charWidth} height={2 * padding + charHeight} stroke-width="2" stroke="#000">
    <!-- Background -->
    <rect width="100%" height="100%" fill="#ddf" stroke="none" />
    <!-- Horizontal line, always present -->
    <line x1={padding} y1={padding + charHeight * .4} x2={padding + charCount * charWidth} y2={padding + charHeight * .4} />
    {#each chars as char, i}
        <!-- Top vertical left -->
        {#if char & 0b1000000000000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth}
                y1={padding + (charHeight * .4) * .3}
                y2={padding + charHeight * .4}
            />
        {/if}
        <!-- Top circle bottom left -->
        {#if char & 0b0100000000000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * .3}
                y2={padding + (charHeight * .4) * .6}
            />
        {/if}
        <!-- Top circle top left -->
        {#if char & 0b0010000000000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * .3}
                y2={padding}
            />
        {/if}
        <!-- Top vertical middle -->
        {#if char & 0b0001000000000000}
            <line
                x1={padding + i * charWidth + charWidth / 2}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding}
                y2={padding + (charHeight * .4) * .6}
            />
        {/if}
        <!-- Top circle bottom right -->
        {#if char & 0b0000100000000000}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * .3}
                y2={padding + (charHeight * .4) * .6}
            />
        {/if}
        <!-- Top circle top right -->
        {#if char & 0b0000010000000000}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * .3}
                y2={padding}
            />
        {/if}
        <!-- Top vertical right -->
        {#if char & 0b0000001000000000}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth}
                y1={padding + (charHeight * .4) * .3}
                y2={padding + charHeight * .4}
            />
        {/if}
        <!-- Top diacritic -->
        {#if char & 0b0000000100000000}
            <line
                x1={padding + i * charWidth + charWidth / 2}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + charHeight * .4}
                y2={padding + (charHeight * .4) * .6}
            />
        {/if}
        <!-- Bottom vertical left -->
        {#if char & 0b0000000010000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth}
                y1={padding + (charHeight * .4) * 1.4}
                y2={padding + (charHeight * .4) * 1.7}
            />
        {/if}
        <!-- Bottom circle bottom left -->
        {#if char & 0b0000000001000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * 1.7}
                y2={padding + (charHeight * .4) * 2}
            />
        {/if}
        <!-- Bottom circle top left -->
        {#if char & 0b0000000000100000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * 1.7}
                y2={padding + (charHeight * .4) * 1.4}
            />
        {/if}
        <!-- Bottom vertical middle -->
        {#if char & 0b0000000000010000}
            <line
                x1={padding + i * charWidth + charWidth / 2}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + charHeight * .4 * 1.4}
                y2={padding + (charHeight * .4) * 2}
            />
        {/if}
        <!-- Bottom circle bottom right -->
        {#if char & 0b0000000000001000}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * 1.7}
                y2={padding + (charHeight * .4) * 2}
            />
        {/if}
        <!-- Bottom circle top right -->
        {#if char & 0b0000000000000100}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * 1.7}
                y2={padding + (charHeight * .4) * 1.4}
            />
        {/if}
        <!-- Bottom vertical right -->
        {#if char & 0b0000000000000010}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth}
                y1={padding + charHeight * .4 * 1.4}
                y2={padding + (charHeight * .4) * 1.7}
            />
        {/if}
        <!-- Bottom diacritic -->
        {#if char & 0b0000000000000001}
            <circle
                cx={padding + i * charWidth + charWidth / 2}
                cy={padding + charHeight * .8 + 5}
                r="4"
                fill="none"
            />
        {/if}
    {/each}
</svg>
<style>
    svg {
        display: block;
    }
</style>
