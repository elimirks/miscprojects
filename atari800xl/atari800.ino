#include "Keyboard.h"

struct Key {
  unsigned int keyCode;
  unsigned int writePin;
  unsigned int readPin;
  unsigned int pressed;
};

// Maps pins to the corresponding Atari 800 XL pinout
const int pin_map[24] = {
  0,  // Atari 800 XL pinout starts at pin 1
  22,
  11,
  12,
  13,

  14,
  15,
  16,
  17,
  18,

  19,
  20,
  21,
  23,
  10,

  9,
  8,
  7,
  0, // Ground line (not connected)
  0, // Not connected

  // TODO: These are the special pins (not part of the main keyboard)
  0, 
  0,
  0,
  0,
};

/*
 * References:
 * http://members.casema.nl/hhaydn/howel/logic/burched/b5_800xl_kbd.htm
 * https://www.cambiaresearch.com/articles/15/javascript-char-codes-key-codes
 * https://www.arduino.cc/en/Reference/KeyboardModifiers
 */
Key key_states[64] = {
  {
    .keyCode = KEY_ESC,
    .writePin = 2,
    .readPin = 17
  },
  {
    .keyCode = KEY_TAB,
    .writePin = 4,
    .readPin = 17
  },
  {
    .keyCode = KEY_LEFT_CTRL,
    .writePin = 5,
    .readPin = 9
  },
  {
    .keyCode = KEY_LEFT_SHIFT,
    .writePin = 8,
    .readPin = 9
  },
  { // SPACE
    .keyCode = 32,
    .writePin = 7,
    .readPin = 11
  },
  { // BACKSPACE
    .keyCode = KEY_BACKSPACE,
    .writePin = 1,
    .readPin = 17
  },
  { // BREAK
    .keyCode = KEY_LEFT_ALT,
    .writePin = 1,
    .readPin = 9
  },
  { // RETURN
    .keyCode = KEY_RETURN,
    .writePin = 3,
    .readPin = 17
  },
  { // CAPS
    .keyCode = KEY_CAPS_LOCK,
    .writePin = 6,
    .readPin = 17
  },
  { // GUI
    .keyCode = KEY_LEFT_GUI,
    .writePin = 7,
    .readPin = 16
  },
  { // HELP
    .keyCode = KEY_F1,
    .writePin = 8,
    .readPin = 11
  },
  { // 1
    .keyCode = 49,
    .writePin = 2,
    .readPin = 16
  },
  { // 2
    .keyCode = 50,
    .writePin = 2,
    .readPin = 15
  },
  { // 3
    .keyCode = 51,
    .writePin = 2,
    .readPin = 14
  },
  { // 4
    .keyCode = 52,
    .writePin = 2,
    .readPin = 13
  },
  { // 5
    .keyCode = 53,
    .writePin = 2,
    .readPin = 12
  },
  { // 6
    .keyCode = 54,
    .writePin = 2,
    .readPin = 10
  },
  { // 7
    .keyCode = 55,
    .writePin = 1,
    .readPin = 10
  },
  { // 8
    .keyCode = 56,
    .writePin = 1,
    .readPin = 12
  },
  { // 9
    .keyCode = 57,
    .writePin = 1,
    .readPin = 13
  },
  { // 0
    .keyCode = 48,
    .writePin = 1,
    .readPin = 14
  },
  { // <
    .keyCode = 45,
    .writePin = 1,
    .readPin = 15
  },
  { // >
    .keyCode = 61,
    .writePin = 1,
    .readPin = 16
  },
  { // q
    .keyCode = 113,
    .writePin = 4,
    .readPin = 16
  },
  { // w
    .keyCode = 119,
    .writePin = 4,
    .readPin = 15
  },
  { // e
    .keyCode = 101,
    .writePin = 4,
    .readPin = 14
  },
  { // r
    .keyCode = 114,
    .writePin = 4,
    .readPin = 13
  },
  { // t
    .keyCode = 116,
    .writePin = 4,
    .readPin = 12
  },
  { // y
    .keyCode = 121,
    .writePin = 4,
    .readPin = 10
  },
  { // u
    .keyCode = 117,
    .writePin = 3,
    .readPin = 10
  },
  { // i
    .keyCode = 105,
    .writePin = 3,
    .readPin = 12
  },
  { // o
    .keyCode = 111,
    .writePin = 3,
    .readPin = 13
  },
  { // p
    .keyCode = 112,
    .writePin = 3,
    .readPin = 14
  },
  { // _
    .keyCode = 91,
    .writePin = 3,
    .readPin = 15
  },
  { // =
    .keyCode = 93,
    .writePin = 3,
    .readPin = 16
  },
  { // a
    .keyCode = 97,
    .writePin = 6,
    .readPin = 16
  },
  { // s
    .keyCode = 115,
    .writePin = 6,
    .readPin = 15
  },
  { // d
    .keyCode = 100,
    .writePin = 6,
    .readPin = 14
  },
  { // f
    .keyCode = 102,
    .writePin = 6,
    .readPin = 13
  },
  { // g
    .keyCode = 103,
    .writePin = 6,
    .readPin = 12
  },
  { // h
    .keyCode = 104,
    .writePin = 6,
    .readPin = 11
  },
  { // j
    .keyCode = 106,
    .writePin = 5,
    .readPin = 11
  },
  { // k
    .keyCode = 107,
    .writePin = 5,
    .readPin = 12
  },
  { // l
    .keyCode = 108,
    .writePin = 5,
    .readPin = 13
  },
  { // ;
    .keyCode = 59,
    .writePin = 5,
    .readPin = 14
  },
  { // +
    .keyCode = 39,
    .writePin = 5,
    .readPin = 15
  },
  { // *
    .keyCode = 92,
    .writePin = 5,
    .readPin = 16
  },
  { // z
    .keyCode = 122,
    .writePin = 8,
    .readPin = 16
  },
  { // x
    .keyCode = 120,
    .writePin = 8,
    .readPin = 15
  },
  { // c
    .keyCode = 99,
    .writePin = 8,
    .readPin = 14
  },
  { // v
    .keyCode = 118,
    .writePin = 8,
    .readPin = 13
  },
  { // b
    .keyCode = 98,
    .writePin = 8,
    .readPin = 12
  },
  { // n
    .keyCode = 110,
    .writePin = 7,
    .readPin = 10
  },
  { // m
    .keyCode = 109,
    .writePin = 7,
    .readPin = 12
  },
  { // ,
    .keyCode = 44,
    .writePin = 7,
    .readPin = 13
  },
  { // .
    .keyCode = 46,
    .writePin = 7,
    .readPin = 14
  },
  { // /
    .keyCode = 47,
    .writePin = 7,
    .readPin = 15
  }
};

void setup() {
  for (unsigned int i = 1; i <= 8; i++) {
    pinMode(pin_map[i], OUTPUT);
    digitalWrite(pin_map[i], HIGH);
  }

  for (unsigned int i = 9; i <= 17; i++) {
    pinMode(pin_map[i], INPUT_PULLUP);
  }

  // initialize control over the keyboard:
  Keyboard.begin();
}

void loop() {
  for (unsigned int i = 0; i < sizeof(key_states) / sizeof(Key); i++) {
    Key *k = &key_states[i];

    digitalWrite(pin_map[k->writePin], LOW);

    int state = digitalRead(pin_map[k->readPin]);

    if (state == LOW && k->pressed == 0) {
      Keyboard.press(k->keyCode);
      k->pressed = 1;
    } else if (state == HIGH && k->pressed == 1) {
      Keyboard.release(k->keyCode);
      k->pressed = 0;
    }

    digitalWrite(pin_map[k->writePin], HIGH);
  }

  // For safety - I had an issue where I was writing key presses too quickly (because of a bug)
  delay(10);
}
