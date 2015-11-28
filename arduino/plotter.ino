#define STATE_COUNT 8
#define WAIT 2
#define PEN_UP_WAIT 500
#define PEN_DOWN_WAIT 200

#define STEP_COUNT 1

#define LEFT_MOTOR 0
#define RIGHT_MOTOR 1

#define PEN_PIN 10

int motorStates[2] =  {
  0, // left
  0  // right
};

int pins[2][4] = {
// N1 N2 N3 N4
  {9, 8, 7, 6}, // left
  {5, 4, 3, 2}  // right
};

int states[STATE_COUNT][4] = {
  {HIGH, LOW,  LOW,  LOW },
  {HIGH, HIGH, LOW,  LOW },
  {LOW,  HIGH, LOW,  LOW },
  {LOW,  HIGH, HIGH, LOW },
  {LOW,  LOW,  HIGH, LOW },
  {LOW,  LOW,  HIGH, HIGH},
  {LOW,  LOW,  LOW,  HIGH},
  {HIGH, LOW,  LOW,  HIGH}
};

void pMode(int motor) {
  pinMode(pins[motor][0], OUTPUT);
  pinMode(pins[motor][1], OUTPUT);
  pinMode(pins[motor][2], OUTPUT);
  pinMode(pins[motor][3], OUTPUT);
}

void outputState(int motor, int *state) {
  digitalWrite(pins[motor][0], state[0]);
  digitalWrite(pins[motor][1], state[1]);
  digitalWrite(pins[motor][2], state[2]);
  digitalWrite(pins[motor][3], state[3]);
}

void outputMotorStateToPins() {
  outputState(LEFT_MOTOR, states[motorStates[LEFT_MOTOR]]);
  outputState(RIGHT_MOTOR, states[motorStates[RIGHT_MOTOR]]);
}

void setup() {
  Serial.begin(115200);
  pMode(LEFT_MOTOR);
  pMode(RIGHT_MOTOR);
  pinMode(PEN_PIN, OUTPUT);

  outputMotorStateToPins();

  delay(1000);
}

void makeSteps(int count, void (*advanceLeftMotorState)(int), void (*advanceRightMotorState)(int)) {
  for (int i=0; i<count; i++) {
    advanceLeftMotorState(LEFT_MOTOR);
    advanceRightMotorState(RIGHT_MOTOR);
    outputMotorStateToPins();
    delay(WAIT);
  }
}

void next(int motor) {
  motorStates[motor] = (motorStates[motor] + 1) % STATE_COUNT;
}

void previous(int motor) {
  motorStates[motor] = (motorStates[motor] - 1 + STATE_COUNT) % STATE_COUNT;
}

void motorLeft(int motor) { previous(motor); };
void motorRight(int motor) { next(motor); };

void stay(int motor) {}

void ln() { makeSteps(STEP_COUNT, motorLeft,  stay); }
void nl() { makeSteps(STEP_COUNT, stay,       motorLeft); }
void rn() { makeSteps(STEP_COUNT, motorRight, stay); }
void nr() { makeSteps(STEP_COUNT, stay,       motorRight); }
void lr() { makeSteps(STEP_COUNT, motorLeft,  motorRight); }
void rl() { makeSteps(STEP_COUNT, motorRight, motorLeft); }
void ll() { makeSteps(STEP_COUNT, motorLeft,  motorLeft); }
void rr() { makeSteps(STEP_COUNT, motorRight, motorRight); }

void penUp() {
  digitalWrite(PEN_PIN, LOW);
  delay(PEN_UP_WAIT);
}

void penDown() {
  digitalWrite(PEN_PIN, HIGH);
  delay(PEN_DOWN_WAIT);
}

void dispatch(char c) {
  switch (c) {
    case 'a': penUp(); break;
    case 'b': penDown(); break;
    case 'c': ln(); break;
    case 'd': nl(); break;
    case 'e': rn(); break;
    case 'f': nr(); break;
    case 'g': lr(); break;
    case 'h': rl(); break;
    case 'i': ll(); break;
    case 'j': rr(); break;
  }
}

void repeat(int n, char step) {
  for(int i=0; i<n; i++) {
    dispatch(step);
  }
}

char c;

void loop() {
  // int r = 200;
  // penDown();
  // repeat(r, 'c');
  // repeat(r, 'd');
  // repeat(r, 'e');
  // repeat(r, 'f');
  // repeat(r, 'g');
  // repeat(r, 'h');
  // repeat(r, 'i');
  // repeat(r, 'j');

  // penUp();
  // delay(1000);
  // penDown();
  // delay(1000);

  if (Serial.available() > 0) {
    c = Serial.read();
    dispatch(c);
    Serial.println(String(c));
    Serial.flush();
  }
}
