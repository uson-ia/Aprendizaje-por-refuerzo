/*  Ejemplo tomado de http://mnemstudio.org/index.php   */

#include <iostream>
#include <iomanip>
#include <ctime>

/*  Este tutorial introduce el concepto de Q-learning
    a traves de un simple ejemplo numerico 
    
    El ejemplo describe un agente el cual usa entrenamiento no supervizado
    para aprender de un medio ambiente no conocido
*/
    

using namespace std;

const int qSize = 6;
const double gamma = 0.8;
const int iterations = 10;
int initialStates[qSize] = {1, 3, 5, 2, 4, 0};

int R[qSize][qSize] =  {{-1, -1, -1, -1, 0, -1},
			{-1, -1, -1, 0, -1, 100},
			{-1, -1, -1, 0, -1, -1},
			{-1, 0, 0, -1, 0, -1},
			{0, -1, -1, 0, -1, 100},
			{-1, 0, -1, -1, 0, 100}};

int Q[qSize][qSize];
int currentState;

void episode(int initialState);
void chooseAnAction();
int getRandomAction(int upperBound, int lowerBound);
void initialize();
int maximum(int state, bool returnIndexOnly);
int reward(int action);

int main(){

	int newState;

	initialize();

    //realiza ensallos de aprendizaje a apartir de todos los estados iniciales
    for(int j = 0; j <= (iterations - 1); j++){
        for(int i = 0; i <= (qSize - 1); i++){
            episode(initialStates[i]);
		}
	}

    //imprime la matriz Q
    for(int i = 0; i <= (qSize - 1); i++){
        for(int j = 0; j <= (qSize - 1); j++){
            cout << setw(5) << Q[i][j];
			if(j < qSize - 1){
				cout << ",";
			}
		}
        cout << "\n";
	}
    cout << "\n";

	//realiza pruebas a partir de todos los estados iniciales
	for(int i = 0; i <= (qSize - 1); i++){
        currentState = initialStates[i];
        newState = 0;
		do {
            newState = maximum(currentState, true);
            cout << currentState << ", ";
            currentState = newState;
        } while(currentState < 5);
        cout << "5" << endl;
	}
    system("pause");
	return 0;
}

void episode(int initialState){

    currentState = initialState;

    //viaja de un estado a otro hasta que se alcanza el estado meta
	do {
        chooseAnAction();
	} while(currentState == 5);

    //cuando currentState=5,corre a travez del conjunto una vez mas a la convergencia
    for(int i = 0; i <= (qSize - 1); i++){
        chooseAnAction();
	}
}

void chooseAnAction(){

	int possibleAction;

    //elige aleatoriamente un aposible accion relacionada don el estado actual
    possibleAction = getRandomAction(qSize, 0);

	if(R[currentState][possibleAction] >= 0){
        Q[currentState][possibleAction] = reward(possibleAction);
        currentState = possibleAction;
	}
}

int getRandomAction(int upperBound, int lowerBound){

	int action;
	bool choiceIsValid = false;
	int range = (upperBound - lowerBound) + 1;

    //elige aleatoriamente un aposible accion relacionada don el estado actual
    do {
        //obtiene un valor aleatorio entre 0 y 6
        action = lowerBound + int(range * rand() / (RAND_MAX + 1.0));
		if(R[currentState][action] > -1){
            choiceIsValid = true;
		}
    } while(choiceIsValid == false);

    return action;
}

void initialize(){

	srand((unsigned)time(0));

    for(int i = 0; i <= (qSize - 1); i++){
        for(int j = 0; j <= (qSize - 1); j++){
            Q[i][j] = 0;
		}
	}
}

int maximum(int state, bool returnIndexOnly){
// si returnIndexOnly = true, se devuelve un índice de matriz Q.
// Si returnIndexOnly = false, se devuelve un elemento de la matriz Q.

	int winner;
	bool foundNewWinner;
	bool done = false;

    winner = 0;
    
	do {
        foundNewWinner = false;
        for(int i = 0; i <= (qSize - 1); i++){
			if((i < winner) || (i > winner)){     //evita compararse con sigo mismo
				if(Q[state][i] > Q[state][winner]){
                    winner = i;
                    foundNewWinner = true;
				}
			}
		}

		if(foundNewWinner == false){
            done = true;
		}

    } while(done = false);

	if(returnIndexOnly == true){
		return winner;
	}else{
		return Q[state][winner];
	}
}

int reward(int action){			
    return static_cast<int>(R[currentState][action] + (gamma * maximum(action, false)));
}
