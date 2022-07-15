// Taken from https://eater.net/6502

const char ADDR[] = {38, 39, 40, 41, 53, 52, 51, 50,
                     49, 48, 47, 46, 45, 44, 43, 42};
const char DATA[] = {37, 36, 35, 34, 33, 32, 31, 30};

#define CLOCK 21
#define READ_WRITE 29

void setup() {
    for (int n = 0; n < 16; n += 1) {
        pinMode(ADDR[n], INPUT);
    }
    for (int n = 0; n < 8; n += 1) {
        pinMode(DATA[n], INPUT);
    }
    pinMode(CLOCK, INPUT);
    pinMode(READ_WRITE, INPUT);

    attachInterrupt(digitalPinToInterrupt(CLOCK), onClock, RISING);
    Serial.begin(57600);
}

void onClock() {
    char output[15];

    unsigned int address = 0;
    for (int n = 0; n < 16; n += 1) {
        int bit = digitalRead(ADDR[n]) ? 1 : 0;
        Serial.print(bit);
        address = (address << 1) + bit;
    }

    Serial.print("   ");

    unsigned int data = 0;
    for (int n = 0; n < 8; n += 1) {
        int bit = digitalRead(DATA[n]) ? 1 : 0;
        Serial.print(bit);
        data = (data << 1) + bit;
    }

    sprintf(output, "   %04x  %c %02x", address, digitalRead(READ_WRITE) ? 'r' : 'w', data);
    Serial.println(output);
}

void loop() {
}
