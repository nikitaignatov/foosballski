int led = 13;     
int threshold = 50; 
bool is_connected[] = {0,0,0,0};

void setup() {
  Serial.begin(9600);
  for  (int i = 8;i<11;i++){
    pinMode(i, OUTPUT);
  }
  pinMode(led, OUTPUT);
}

void loop() {
  for  (int i = 8;i<11;i++){
    digitalWrite(i, HIGH);
  } 
  for  (int i = 0; i < 1; i++){
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
    digitalWrite(led, result ? LOW : HIGH);
    Serial.println(
      String("{ command: \"" ) 
      + String(result ? "connected" : "disconnected" )
      + String("\", trap_id: \"A") + pin + String("\" }")
    ); 
}
