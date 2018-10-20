int led = 13;     
int threshold = 50; 

int sensor_count = 4;
int power_pin_from = 8;
unsigned long time;

bool is_connected[] = {0,0,0,0};

void setup() {
  Serial.begin(9600);
  for  (int i = power_pin_from ; i < power_pin_from + sensor_count; i++){
    pinMode(i, OUTPUT);
  }
  pinMode(led, OUTPUT);
}

void loop() {
  for  (int i = power_pin_from ; i < power_pin_from + sensor_count; i++){
    digitalWrite(i, HIGH);
  } 
  for  (int i = 0; i < sensor_count; i++){
    bool state = is_connected[i];
    bool result = detect(i , state); 
    if(state != result){
      is_connected[i] = result;     
      send(i, result);
    }
  }    
}

int detect(int pin, bool is_connected )
{  
  int sensorValue = analogRead(pin);  
  if(is_connected && sensorValue > threshold) {
    return false;
  }
  else if(!is_connected && sensorValue <= threshold) {
    return true;
  }
  return is_connected;
}

void send(int pin, bool result)
{    
  time = micros();
  Serial.println(
    String("{ event: \"" ) 
    + String(result ? "connected" : "disconnected" )
    + String("\", data: { trap_id: \"A") 
    + pin 
    + String("\", ")
    + time
    + String("} }")
  );   
  digitalWrite(led, result ? LOW : HIGH);
}
