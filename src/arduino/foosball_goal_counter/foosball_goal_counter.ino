bool started = false;
unsigned long time;

int led = 13;
int threshold = 50;

int sensor_count = 1;
int power_pin_from = 2;
bool is_connected[] = {0, 0, 0, 0};


void setup() {
  Serial.begin(9600);
  for  (int i = power_pin_from ; i < power_pin_from + (sensor_count * 2); i++) {
    pinMode(i, OUTPUT);
  }
  pinMode(led, OUTPUT);
}

void loop() {
  commands();
  if (started) {
    for  (int i = 0; i < sensor_count; i++) {
      bool state = is_connected[i];
      bool result = detect(i , state);
      if (state != result) {
        is_connected[i] = result;
        time = millis();
        send(i, result);
      }
    }
  }
}

void commands() {
  while (Serial.available() > 0) {
    String cmd = Serial.readStringUntil('\n');
    if (cmd.startsWith("start")) {
      started = true;
      toggle(HIGH);
      Serial.println("{ \"Case\": \"Started\" }");
    }
    else if (cmd.startsWith("stop")) {
      started = false;
      
      for  (int i = 0; i < sensor_count; i++) {
          is_connected[i] = false;
      }
      toggle(LOW);
      Serial.println("{ \"Case\": \"Stopped\" }");
    }
    else if (cmd.startsWith("test")) {
      for  (int i = 0; i < sensor_count; i++) {
        bool state = is_connected[i];
        send(i, state);
      }
    }
  }
}

int detect(int pin, bool is_connected ) {
  int sensorValue = analogRead(pin);
  if (is_connected && sensorValue > threshold) {
    return false;
  }
  else if (!is_connected && sensorValue <= threshold) {
    return true;
  }
  return is_connected;
}

void toggle(int mode) {
  for (int i = power_pin_from ; i < power_pin_from + (sensor_count * 2); i++) {
    digitalWrite(i, mode);
    Serial.println(
      String("{ \"Case\": \"PinReading\", \"Fields\": [ ")
      + (i)
      + String(",  {\"Case\": \"")
      + String(mode ? "On" : "Off")
      + String("\" }] }")
    );
  }
}

void send(int pin, bool result)
{
  Serial.println(
    String("{ \"Case\": \"SensorReading\", \"Fields\": [\"A" )
    + pin
    + String("\", {\"Case\": \"")
    + String(result ? "Connected" : "Disconnected" )
    + String("\"},")
    + time
    + String(" ] }")
  );
  digitalWrite(led, result ? LOW : HIGH);
}
