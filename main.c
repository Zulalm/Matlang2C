#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
char *scalars;
char *vectors;
char *matrices;
char *allVariables;
//stack initialization for infix to postfix
char *exprStack[256];
char * termStack[256];
char *factorStack[256];
char* tempStack[256];
int tempTop =-1;
int factorTop = -1;
int termTop = -1;
int exprTop = -1;

//Detecting compile time errors and printing line number which occurs to error
void error(int lineNum,FILE * fp,FILE *fw){
    printf("Error (Line %d)\n", lineNum);
    fclose(fp);
    fclose(fw);
    remove("file.c");
    exit(0);
}
//stack functions
char* pop(char*stack[],int*top){
    if(*top == -1){
        return "";
    }
    char *s = (char*) calloc(256,1);
    s = stack[*top];
    *top -=1;
    return s;
}
char * peek(char*stack[],int*top){
    if(*top==-1){
        return "";
    }
    return stack[*top];
}
void push(char*stack[],int*top,char* s){
    *top +=1;
    stack[*top] = s;
}
//Checking if the parameter is a number
int isNumber(char* str){
    int virgul = 0;
    for (char *c = str; *c;c++){
        if(isdigit(*c)==0) {

            if (*c == '.' && virgul == 0) {
                virgul = 1;
            } else {
                return 0;
            }
        }
    }

    return 1;
}
//Finding the last occurrence of delim parameter in a str parameter
char * strRevTok(char*str,char delim){
    if(str == NULL){
        return NULL;
    }
    char *q;
    char * new = (char*) calloc(256,1);
    q = strrchr(str,delim);
    if (q==NULL){
        return str;
    }
    else{
        strncat(new,str,strlen(str)-strlen(q));
        return new;
    }

}
//Finding the first occurrence of delim parameter in a str parameter
char *myTok(char*str,char delim){
    char *q;
    char * new = (char*) calloc(256,1);
    q = strchr(str,delim);
    if( q == NULL){
        return str;
    }
    else{
        strncat(new,str,strlen(str)-strlen(q));
        return new;
    }
}
//Combining the characters in the str and destination parameters into n characters.
char* strRevCat(char*destination,char* str){
    char *n = (char*)calloc(256,1);
    strcpy(n,str);
    strcat(n,destination);
    return n;
}
//Parsing terms according to these chars: "(", "[", "-", "+", "*", "]", ")"
char * parseTerm(char*str){
    if(str == NULL){
        return NULL;
    }
    int pr = 0;
    int k = 0;
    char *q;
    char * new = (char*) calloc(256,1);
    for(q=str ; *q;q++){
        if(*q=='('){
            pr++;
        }
        else if(*q=='['){
            k++;
        }
        else if((*q=='-' || *q == '+'||*q == '*')&& !pr && !k){
            if (q==NULL){
                return str;
            }
            else{
                strncat(new,str,strlen(str)-strlen(q));
                return new;
            }
        }
        else if(*q==']'){
            k--;
        }
        else if(*q==')'){
            pr--;
        }
    }
    return str;
}
//Counting square brackets and brackets and push operation chars and str parameter to temp stacks
void parse(char* str){
    if (str ==NULL){
        return;
    }
    int koseli = 0;
    int prntz = 0;
    for(char*c = str; *c; c++){
        if(*c == '+' && koseli ==0 && prntz==0){
            push(tempStack,&tempTop, parseTerm(str));
            push( tempStack,&tempTop,"+");
            str = c+1;
        }
        else if(*c == '-'&& koseli ==0 && prntz==0){
            push(tempStack,&tempTop, parseTerm(str));
            push( tempStack,&tempTop,"-");
            str = c+1;
        }
        else if(*c == '*'&& koseli ==0 && prntz==0){
            push(tempStack,&tempTop,parseTerm(str));
            push( tempStack,&tempTop,"*");
            str = c+1;
        }
        else if(*c == '['){
            koseli++;
        }
        else if(*c == '('){
            prntz++;

        }
        else if(*c == ']'){
            koseli--;
            if(*(c+1) == '\0'){
                push(tempStack,&tempTop,str);
                str = c+1;
            }
        }
        else if(*c == ')'){
            prntz--;
            if(*(c+1) == '\0'){
                push(tempStack,&tempTop,str);
                str = c+1;
            }
        }
        else if(*(c+1) == '\0'){
            push(tempStack,&tempTop,str);
            str = c+1;
        }

    }
    while(tempTop > -1){
        push(termStack,&termTop,(pop(tempStack,&tempTop)));
    }
}
//Taking the expression as factor or morefactors, checking it and doing a matrix or scalar multiplication.
char*factor(int lineNum,FILE * fw,FILE * fp){
    char * right = pop(factorStack,&factorTop);
    char * sign = pop(factorStack,&factorTop);
    if (sign == NULL || strcmp(sign,"")==0){
        return right;
    }
    else{
        char * left = factor(lineNum,fw,fp);
        if(left == NULL){
            return right;
        }
        char* leftcpy = (char*) calloc(256,1);
        char* rightcpy = (char*) calloc(256,1);
        strcpy(leftcpy,left);
        leftcpy = strRevCat(leftcpy,"\n");
        strcat(leftcpy,"\n");
        strcpy(rightcpy,right);
        rightcpy = strRevCat(rightcpy,"\n");
        strcat(rightcpy,"\n");
        if(strstr(scalars,leftcpy)!=NULL && strstr(scalars,rightcpy)!=NULL){
            strcat(left,"*");
            strcat(left,right);
            return left;
        }
        strtok(rightcpy,"\n");
        //rightcpy = strRevCat(rightcpy,"\n");
        strcat(rightcpy,"[");
         if(strstr(scalars,leftcpy)!=NULL && strstr(matrices,rightcpy)!=NULL){
            char* name = (char*) calloc(256,1);
            strcpy(name,"_");
            strcpy(name,left);
            strcpy(name,right);
            strcpy(rightcpy,right);
            strcat(rightcpy,"[");
            char* row1 = strstr(matrices,rightcpy);
            row1 = strchr(row1,'[');
            row1++;
            char * col1 = strdup(row1);
            row1 = myTok(row1,']');
            col1 = strchr(col1,'[');
            col1++;
            col1 = myTok(col1,']');
            sprintf(name,"%s[",name);
            if(strstr(matrices,name)==NULL){
                sprintf(name,"%s%s][%s]",name,row1,col1);
                fprintf(fw,"float %s;\n",name);
            }
            name = myTok(name,'[');
            fprintf(fw,"matrixScalarMultiply((float *)%s, %s,%s, %s,(float*));\n",right,left,row1,col1,name);
            return name;
        }
         strcpy(rightcpy,right);
        strcpy(leftcpy,left);
        leftcpy = strRevCat(leftcpy,"\n");
        strcat(leftcpy,"[");
        strcat(rightcpy,"\n");
        rightcpy = strRevCat(rightcpy,"\n");
        if(strstr(scalars,rightcpy)!=NULL && strstr(matrices,leftcpy)!=NULL){
            char* name = (char*) calloc(256,1);
            strcpy(name,"_");
            strcpy(name,right);
            strcpy(name,left);
            strcpy(leftcpy,left);
            strcat(leftcpy,"[");
            char* row1 = strstr(matrices,leftcpy);
            row1 = strchr(row1,'[');
            row1++;
            char * col1 = strdup(row1);
            row1 = myTok(row1,']');
            col1 = strchr(col1,'[');
            col1++;
            col1 = myTok(col1,']');
            sprintf(name,"%s[",name);
            if(strstr(matrices,name)==NULL){
                sprintf(name,"%s%s][%s]",name,row1,col1);
                fprintf(fw,"float %s;\n",name);
            }
            name = myTok(name,'[');
            fprintf(fw,"matrixScalarMultiply((float *)%s, %s,%s, %s,(float*));\n",left,right,row1,col1,name);
            return name;
        }
        strcpy(rightcpy,right);
        strcpy(leftcpy,left);
        leftcpy = strRevCat(leftcpy,"\n");
        strcat(leftcpy,"[");
        strcat(rightcpy,"[");
        rightcpy = strRevCat(rightcpy,"\n");
         if(strstr(matrices,rightcpy)!=NULL && strstr(matrices,leftcpy)!=NULL){
            char* row1 = strstr(matrices,leftcpy);
            row1 = strchr(row1,'[');
            row1++;
            char * col1 = strdup(row1);
            row1 = myTok(row1,']');
            col1 = strchr(col1,'[');
            col1++;
            col1 = myTok(col1,']');
            char* row2 = strstr(matrices,rightcpy);
            row2 = strchr(row2,'[');
            row2++;
            char * col2 = strdup(row2);
            row2 = myTok(row2,']');
            col2 = strchr(col2,'[');
            col2++;
            col2 = myTok(col2,']');
            strtok(row1," ");strtok(row2," ");strtok(col1," ");strtok(col2," ");
            if(strcmp(col1,row2)==0){
                char* buffer = (char*) calloc(265,1);

                sprintf(buffer,"\n_%s_c_%s[",left,right);
                if(strstr(matrices,buffer)==NULL){
                    sprintf(buffer,"_%s_c_%s[%s][%s]\n",left,right,row1,col2);
                    strcat(matrices,buffer);
                    sprintf(buffer,"_%s_c_%s[%s][%s]",left,right,row1,col2);
                    fprintf(fw,"float %s;\n",buffer);

                }
                sprintf(buffer,"_%s_c_%s",left,right);
                //strtok(buffer,"[");
                fprintf(fw,"matrixMultiply((float *)%s, (float *)%s,(float *)%s, %s, %s,%s);\n",left,right,buffer,row1,col1,col2);

                return buffer;

            }
            else{
                error(lineNum,fp,fw);
            }
        }

    }
    return  right;
}
//Taking the parsed expression and separating it into factors and running the factor function, while doing the necessary controls and perform operations.
char* term(int lineNum, FILE* fw,FILE* fp){
    while(strcmp(peek(termStack,&termTop),"+")!=0 && strcmp(peek(termStack,&termTop),"-")!=0 && termTop>-1) {
        if (strcmp(peek(termStack,&termTop), "") == 0 || strcmp(peek(termStack,&termTop), " ") == 0) {
            pop(termStack,&termTop);
        } else {
            push(factorStack, &factorTop, pop(termStack, &termTop));
        }
    }
    char * sign = pop(termStack,&termTop);
    if(sign!=NULL && strcmp(sign,"")!=0){
        char * leftterm = factor(lineNum,fw,fp);
        char * rightterm = term(lineNum,fw,fp);
        if(rightterm== NULL|| strcmp(rightterm,"")==0){
            return leftterm;
        }
        if(leftterm== NULL || strcmp(leftterm,"")==0){
            return rightterm;
        }
        char * rightcpy = (char*)calloc(256,1);
        char * leftcpy = (char*)calloc(256,1);
        sprintf(rightcpy,"\n%s\n",rightterm);
        sprintf(leftcpy,"\n%s\n",leftterm);
        if((strstr(scalars,leftcpy)!=NULL || isNumber(leftterm)==1) && (strstr(scalars,rightcpy)!=NULL|| isNumber(rightterm)==1)){
            strcat(leftterm,sign);
            strcat(rightterm,")");
            rightterm = strRevCat(rightterm,"(");
            strcat(leftterm,rightterm);
            char* cpy =  (char*)calloc(strlen(leftterm)+1,1);
            strcpy(cpy,leftterm);
            strcat(cpy,"\n");
            strcat(scalars,cpy);
            return leftterm;
        }
        sprintf(rightcpy,"\n%s[",rightterm);
        sprintf(leftcpy,"\n%s[",leftterm);
        if(strstr(matrices,leftcpy)!=NULL && strstr(matrices,rightcpy)!=NULL){
            if (strcmp(sign,"+")==0){
                sign = "_a_";
            }
            else{
                sign = "_e_";
            }
            char* row1 = strstr(matrices,leftcpy);
            row1 = strchr(row1,'[');
            row1++;
            char * col1 = strdup(row1);
            row1 = myTok(row1,']');
            col1 = strchr(col1,'[');
            col1++;
            col1 = myTok(col1,']');
            char* row2 = strstr(matrices,rightcpy);
            row2 = strchr(row2,'[');
            row2++;
            char * col2 = strdup(row2);
            row2 = myTok(row2,']');
            col2 = strchr(col2,'[');
            col2++;
            col2 = myTok(col2,']');
            strtok(row1," ");strtok(row2," ");strtok(col1," ");strtok(col2," ");
            if(strcmp(row1,row2)==0 && strcmp(col1,col2)==0){
                char * new = (char*) calloc(256,1);
                strcat(new,leftterm);
                strcat(new,sign);
                strcat(new,rightterm);
                new = strRevCat(new,"_");
                char* cpy =  (char*)calloc(256,1);
                sprintf(cpy,"%s[%s][%s]",new,row1,col1);
                char* mat = strdup(cpy);
                sprintf(mat,"\n%s[",new);
                if(strstr(matrices,mat)==NULL) {
                    fprintf(fw,"float %s;\n",cpy);
                    sprintf(cpy,"%s[%s][%s]\n",new,row1,col1);
                    strcat(matrices, cpy);

                }
                strcpy(cpy,new);
                strcat(cpy,"\n");


                if(strcmp(sign,"_a_")==0){
                    fprintf(fw, "matrixAddition((float*)%s,(float*)%s,(float*)%s,%s,%s);\n", leftterm, rightterm, new,
                            row1, col1);
                }
                else{
                    fprintf(fw, "matrixSubtraction((float*)%s,(float*)%s,(float*)%s,%s,%s);\n", leftterm, rightterm, new,
                            row1, col1);
                }
                return new;
            }
            else{
                error(lineNum,fp,fw);
            }



        }
        else{
            error(lineNum,fp,fw);
        }
    }
    else{
        return factor(lineNum,fw,fp);
    }
    return factor(lineNum,fw,fp);
}
//Parsing and controling the whole expression, performing the necessary operations, dividing it into terms and running the term function with those terms.
char * expr(char * str,int lineNum,FILE* fw ,FILE*fp){
    char* token= (char*)calloc(256,1); char*str1 = (char*)calloc(256,1);
    // ( , [ ) ] + - *
    for (char* c = str; *c;c++){
        if(*c == '('){
            push(exprStack,&exprTop,myTok(str,'('));
            push(exprStack,&exprTop,"(");
            str = c+1;
        }
        else if(*c == '['){
            push(exprStack,&exprTop,myTok(str,'['));
            push(exprStack,&exprTop,"[");
            str = c+1;
        }
        else if(*c == '*'){
            push(exprStack,&exprTop,myTok(str,'*'));
            push(exprStack,&exprTop, "*");
            str = c+1;
        }
        else if(*c == '+'){
            push(exprStack,&exprTop, myTok(str,'+'));
            push(exprStack,&exprTop,"+");
            str = c+1;
        }
        else if(*c == '-'){
            push(exprStack,&exprTop, myTok(str,'-'));
            push(exprStack,&exprTop,"-");
            str = c+1;
        }
        else if(*c == ','){
            push(exprStack,&exprTop,myTok(str,','));
            push(exprStack,&exprTop, ",");
            str = c+1;
        }
        else if(*c == ')'){
            push(exprStack,&exprTop,myTok(str,')'));
            str1 =strcpy(str1,"");
            int choose = 0;
            int ex1 = 0;int ex2 = 0;int ex3 = 0;int ex4 = 1;
            char*expr1=(char*)calloc(256,1);char*expr2=(char*)calloc(256,1);char*expr3= (char*)calloc(256,1);char*expr4 = (char*)calloc(256,1);
            while(strcmp(peek(exprStack,&exprTop),"(")!=0 && exprTop!=-1){
                token = pop(exprStack,&exprTop);
                if(strcmp(token,",")==0 && ex4){
                    expr4 = strcpy(expr4,str1);
                    parse(expr4);
                    expr4 = term(lineNum,fw,fp);
                    str1 = strcpy(str1,"");
                    ex4 = 0;
                    ex3 = 1;
                    choose = 1;
                }
                else if(strcmp(token,",")==0 && ex3){
                    expr3 = strcpy(expr3,str1);
                    parse(expr3);
                    expr3 = term(lineNum,fw,fp);
                    str1 = strcpy(str1,"");
                    ex3 = 0;
                    ex2 = 1;
                }
                else if(strcmp(token,",")==0 && ex2){
                    expr2 = strcpy(expr2,str1);
                    parse(expr2);
                    expr2 = term(lineNum,fw,fp);
                    str1 = strcpy(str1,"");
                    ex2 = 0;
                    ex1 = 1;
                }
                else{
                    str1 = strRevCat(str1,token);
                }
            }
            if(ex1 && choose){
                expr1 = strcpy(expr1,str1);
                parse(expr1);
                expr1 = term(lineNum,fw,fp);
                str1 = strcpy(str1,"");

            }
            if(exprTop <=-1){
                error(lineNum,fp,fw);
            }
            pop(exprStack,&exprTop);
            if(strcmp(peek(exprStack,&exprTop),"tr")==0){
                pop(exprStack,&exprTop);
                char* new = calloc(256,1);
                parse(str1);
                str1 = term(lineNum,fw,fp);
                new = strcpy(new,str1);
                new = strRevCat(new,"_");
                strcat(new,"tr");
                char* cpy =(char*) calloc(256,1);
                cpy = strcpy(cpy,str1);
                strcat(cpy,"[");
                if(strstr(matrices,cpy)!=NULL){
                    char* row1 = strstr(matrices,cpy);
                    row1 = strchr(row1,'[');
                    row1++;
                    char * col1 = strdup(row1);
                    row1 = myTok(row1,']');
                    col1 = strchr(col1,'[');
                    col1++;
                    col1 = myTok(col1,']');
                    char * buffer =(char*) calloc(256,1);
                    sprintf(buffer,"\n%s[%s][%s]\n",new,col1,row1);
                    push(exprStack,&exprTop,new);
                    if(strstr(matrices,buffer)==NULL) {
                        strcat(buffer,"\n");
                        strcat(matrices, buffer);
                        //strcat(allVariables, buffer);
                        fprintf(fw,"float %s[%s][%s];\n",new,col1,row1);
                        fprintf(fw,"tr((float*)%s,%s,%s,(float*)%s);\n",str1,row1,col1,new);

                    }
                    //free(buffer);
                }
                else{
                    error(lineNum,fp,fw);
                }

            }
            else if (strcmp(peek(exprStack,&exprTop),"sqrt")==0){
                pop(exprStack,&exprTop);
                parse(str1);
                str1 = term(lineNum,fw,fp);
                char* cpy = (char*) calloc(256,1);
                strcpy(cpy,str1);
                cpy = strRevCat(cpy,"\n");
                strcat(cpy,"\n");
                if(isNumber(str1)==1 || strstr(scalars,cpy)!=NULL) {

                    str1 = strRevCat(str1, "sqrt(");
                    strcat(str1, ")");
                    strcpy(cpy,str1);
                    push(exprStack,&exprTop,cpy);
                    strcat(str1,"\n");
                    str1 = strRevCat(str1,"\n");
                    if(strstr(scalars,str1)==NULL) {
                        strcat(scalars, str1);
                        //strcat(allVariables, str1);
                    }
                }

                else{
                    error(lineNum,fp,fw);
                }

            }

            else if(strcmp(peek(exprStack,&exprTop),"choose")==0){
                pop(exprStack,&exprTop);
                char * cpy1 = (char*) calloc(256,1);
                char * cpy2 = (char*) calloc(256,1);
                char * cpy3 = (char*) calloc(256,1);
                char * cpy4 = (char*) calloc(256,1);
                sprintf(cpy1,"\n%s\n",expr1);
                sprintf(cpy2,"\n%s\n",expr2);
                sprintf(cpy3,"\n%s\n",expr3);
                sprintf(cpy4,"\n%s\n",expr4);
                if((isNumber(expr1)==1 || strstr(scalars,cpy1)!=NULL) && (isNumber(expr2)==1 || strstr(scalars,cpy2)!=NULL)
                   &&(isNumber(expr3)==1 || strstr(scalars,cpy3)!=NULL)
                   &&(isNumber(expr4)==1 || strstr(scalars,cpy4)!=NULL)) {
                    expr1 = strRevCat(expr1,"choose(");
                    strcat(expr1,",");
                    strcat(expr1,expr2);
                    strcat(expr1,",");
                    strcat(expr1,expr3);
                    strcat(expr1,",");
                    strcat(expr1, expr4);
                    strcat(expr1, ")");
                    ////////////////////////////////////////////////
                    char* neww = (char*) calloc(256,1);
                    push(exprStack,&exprTop,expr1);
                    sprintf(neww,"\n%s\n",expr1);
                    if(strstr(scalars,neww)==NULL) {
                        strcat(scalars, neww);
                        //strcat(allVariables, str1);
                    }
                }
                else{
                    error(lineNum,fp,fw);
                }
            }
            else{
                parse(str1);
                str1 = term(lineNum,fw,fp);
                char * cpy = (char*) calloc(256,1);
                strcpy(cpy,str1);
                strcat(cpy,"\n");
                cpy = strRevCat(cpy,"\n");
                if(strstr(scalars,cpy)!=NULL) {
                    str1 = strRevCat(str1, "(");
                    strcat(str1, ")");
                    strcpy(cpy,str1);
                    strcat(str1,"\n");
                    str1 = strRevCat(str1,"\n");
                    strcat(scalars,str1);
                    push(exprStack,&exprTop,cpy);
                }
                else{
                    strcpy(cpy,str1);
                    push(exprStack,&exprTop,cpy);
                }
            }
            str = c+1;
        }
        else if(*c == ']'){
            push(exprStack,&exprTop, myTok(str,']'));
            str1 =strcpy(str1,"");
            char* left = (char*) calloc(256,1);
            char* right = (char*) calloc(256,1);
            int r=0;
            while(strcmp(peek(exprStack,&exprTop),"[")!=0 && exprTop !=-1){
                token = pop(exprStack,&exprTop);
                if(strcmp(peek(exprStack,&exprTop),",")==0 && r ==0){
                    str1 = strRevCat(str1,token);
                    parse(str1);
                    str1 = term(lineNum,fw,fp);
                    char* cpy = calloc(256,1);
                    strcpy(cpy,str1);
                    strcat(cpy,"\n");
                    cpy = strRevCat(cpy,"\n");
                    if(isNumber(str1)==1 || strstr(scalars,cpy)!=NULL) {
                        right = strRevCat(str1, "-1)][(int)(");
                        strcat(right,"-1)]");
                    }
                    else{
                        error(lineNum,fp,fw);
                    }
                    strcpy(str1,"");
                    r =1;
                    pop(exprStack,&exprTop);
                }
                else{
                    str1 = strRevCat(str1,token);
                }
            }
            parse(str1);
            left = term(lineNum,fw,fp);

            if(exprTop <=-1){
                error(lineNum,fp,fw);
            }
            pop(exprStack,&exprTop);
            if(exprTop <=-1){
                error(lineNum,fp,fw);
            }

            char* matrix = pop(exprStack,&exprTop);
            strcat(matrix,"[");
            matrix = strRevCat(matrix,"\n");
            if(strcmp(right,"")==0){
                if(strstr(vectors,matrix)!=NULL){
                    left = strRevCat(left,"(int)(");
                    strcat(left,"-1)][0]");
                }
                else{
                    error(lineNum,fp,fw);
                }
            }
            else{
                left = strRevCat(left,"(int)(");
                strcat(left,right);
            }
            strcpy(str1,left);


            if(strstr(matrices,matrix)==NULL){
                error(lineNum,fp,fw);
            }
            matrix++;
            strtok(matrix,"\n");
            str1 = strRevCat(str1,matrix);
            char *cpy = (char*) calloc(256,1);
            cpy = strcpy(cpy,str1);
            push(exprStack,&exprTop,cpy);
            strcat(str1,"\n");
            str1 = strRevCat(str1,"\n");
            if(strstr(scalars,str1)==NULL) {
                strcat(scalars, str1);
            }
            str = c+1;
        }
        else if(*c ==' '){
            push(exprStack,&exprTop,myTok(str,' '));
            str = c+1;

        }
        else if(*(c+1) == '\0'){
            push(exprStack,&exprTop,str);
        }
        else{
            //push(stack,top, c);
        }

    }
    //free(token);
    //free(str1);
    while(exprTop >-1){
        push(termStack,&termTop,pop(exprStack,&exprTop));
    }
    return term(lineNum,fw,fp);
}
//A helper function for parsing FOR statements
char * parseFor(char*str){
    if(str == NULL){
        return NULL;
    }
    int pr = 0;
    int k = 0;
    char *q;
    char * new = (char*) calloc(256,1);
    for(q=str ; *q;q++){
        if(*q=='('){
            pr++;
        }
        else if(*q=='['){
            k++;
        }
        else if(*q==',' && !pr && !k){
            if (q==NULL){
                return str;
            }
            else{
                strncat(new,str,strlen(str)-strlen(q));
                return new;
            }
        }
        else if(*q==']'){
            k--;
        }
        else if(*q==')'){
            pr--;
        }
    }
    return str;
}
//Read and parse the input and print the functions with parameters inside the input.
int main(int argc,char *argv[]) {
    FILE *fw;
    scalars = (char*) calloc(10000,1);
    strcpy(scalars,"\n");
    vectors =(char*) calloc(10000,1);
    strcpy(vectors,"\n");
    matrices= (char*) calloc(10000,1);
    strcpy(matrices,"\n");
    allVariables = (char*) calloc(10000,1);
    strcpy(allVariables,"\n");
    if ((fw=fopen("file.c", "w")) == NULL) {
        printf("Cannot open!\n");
        exit(1);
    }
    FILE * fp;
    char line[256];
    if (argc != 2) {

        printf("Give filename as command line argument\n") ;

        return(1);

    }
    fp = fopen(argv[1], "r");

    if(fp == NULL) {

        printf("Cannot open %s\n",argv[1]);

        return(1);

    }
    //Functions printed on output
    fprintf (fw,"#include <stdio.h>\n#include <math.h>\n#include <stdlib.h>\n#include <string.h>\n"
                "float choose(float number1, float number2, float number3, float number4){\n"
                "    if(number1 == 0){\n"
                "        return number2 ;\n"
                "    }\n"
                "    else if (number1 > 0) {\n"
                "        return number3 ;\n"
                "    }\n"
                "    else if (number1 < 0) {\n"
                "    return number4 ;\n"
                "    }\n"
                "    return 0;\n"
                "}\n"

                "void printsep(){\n"
                "    printf(\"------------\\n\");\n"
                "}\n"
                "void (assign(float *first, float *result,int m, int p)) {\n"
                "    int c, k;\n"
                "    for (c = 0; c < m; c++) {\n"
                "        for (k = 0; k < p; k++) {\n"
                "            *((result + c*p)+k) = (*((first + c * p) + k));\n"
                "        }\n"
                "    }\n"
                "}\n"

                "void printScalar(float a){\n"
                "    if(fmod(a,1)==0){\n"
                "        printf(\"%s\\n\",a);    \n"
                "    }\n"
                "    else{\n"
                "        printf(\"%s\\n\",a);\n"
                "    }\n"
                "    \n"
                "}\n"
                "void printMatrix(float *matrix,int row,int column){\n"
                "    for (int i=0; i<row;i++){\n"
                "        for(int j =0;j<column;j++){\n"
                "            if(fmod(*(matrix+i*column+j),1)==0){\n"
                "                printf(\"%s \",*(matrix+i*column+j));\n"
                "            }\n"
                "            else{\n"
                "                printf(\"%s \",*(matrix+i*column+j));\n"
                "            }\n"
                "        }\n"
                "        printf(\"\\n\");\n"
                "    }\n"
                "}\n"
                "void (matrixMultiply(float *first, float *second,float *multiply,int m, int p,int q)) {\n"
                "    int c, d, k;\n"
                "    float  sum = 0;\n"
                "    for (c = 0; c < m; c++) {\n"
                "        for (d = 0; d < q; d++) {\n"
                "            for (k = 0; k < p; k++) {\n"
                "                sum = sum + (*((first + c * p) + k)) * (*((second + k * q) + d));\n"
                "            }\n"
                "\n"
                "            *((multiply + c*q)+d) = sum;\n"
                "            sum = 0;\n"
                "        }\n"
                "    }\n"

                "}\n"
                "void tr(float * mat, int row , int col,float * trans){\n"
                "    int i, j;\n"
                "    // create transpose\n"
                "    for(i = 0; i < row; i++){\n"
                "        for(j = 0; j < col; j++){\n"
                "            *((trans + j*row)+i) = *((mat + i*col)+j);\n"
                "        }\n"
                "    }\n"
                "}\n"
                "char *myTok(char*str,char delim){\n"
                "    char *q;\n"
                "    char * new = (char*) calloc(256,1);\n"
                "    q = strchr(str,delim);\n"
                "    if( q == NULL){\n"
                "        return str;\n"
                "    }\n"
                "    else{\n"
                "        strncat(new,str,strlen(str)-strlen(q));\n"
                "        return new;\n"
                "    }\n"
                "}\n"
                "void(matrixScalarMultiply(float *first, float scalar,int m, int p,float*result)) {\n"
                "    int col,row;\n"
                "    for(row = 0; row < m; row++){\n"
                "        for(col = 0; col< p; col++){\n"
                "            *((result + row*p)+col) = *((first + row*p)+col)*scalar;\n"
                "        }\n"
                "    }\n"

                "}\n"
                "void (matrixAddition(float *first, float *second,float *result,int m, int p)) {\n"
                "    int c, k;\n"
                "    for (c = 0; c < m; c++) {\n"
                "            for (k = 0; k < p; k++) {\n"
                "                *((result + c*p)+k) = (*((first + c * p) + k)) + (*((second + c * p) + k));\n"
                "            }\n"
                "    }\n"
                "}\n"
                "void (initMatrix(char* str,float *result,int m, int p)) {\n"
                "    int c, k;\n"
                "    char*s = str;\n"
                "    while(*s==' '){\n"
                "        s++;\n"
                "    }\n"
                "    for (c = 0; c < m; c++) {\n"
                "        for (k = 0; k < p; k++) {\n"
                "            *((result + c*p)+k) = atof(myTok(s,' '));\n"
                "            s = strchr(s,' ');\n"
                "            if(s) {\n"
                "                while (*s == ' ') {\n"
                "                    s++;\n"
                "                }\n"
                "            }\n"
                "        }\n"
                "    }\n"
                "}\n"
                "void (matrixSubtraction(float *first, float *second,float *result,int m, int p)) {\n"
                "    int c, k;\n"
                "    for (c = 0; c < m; c++) {\n"
                "            for (k = 0; k < p; k++) {\n"
                "                *((result + c*p)+k) = (*((first + c * p) + k)) - (*((second + c * p) + k));\n"
                "            }\n"
                "    }\n"
                "}\n"
                "int main(){\n","%.0f","%.6f","%.0f","%.6f");
    //line number counter for error
    int lineNum = 1;
    char *q =  (char*) calloc(256,1);
    char *token = (char*) calloc(256,1);
    char *name = (char*) calloc(256,1);
    int f= 0;
    while( fgets(line,256,fp) != NULL) {

        char *c= line;
        while(*c== ' ' && *c){
            c++;
        }
        if (*c=='\n' || *c=='\0'){
            lineNum++;
            continue;
        }
        q = strcpy(q,c) ;
        token = strtok(q," ");

        while(*c== ' ' && *c){
            c++;
        }
        if (*c=='\n' || *c=='\0'){
            lineNum++;
            continue;
        }
        if (*c == '#'){
            lineNum++;
            continue;
        }
        //scalar defining 
        else if (strcmp(token,"scalar") == 0){
            c+=6;
            token = strcpy(token,c) ;
            token = strtok(token,"\n");
            token = strtok(token," ");
            while(*c ==' ' && *c){
                c++;
            }
            c+=(strlen(token));
            name = strcpy(name,token);
            strcat(name,"\n");
            name = strRevCat(name,"\n");
            if (*token){
                if(strstr(allVariables,name)!= NULL){
                    error(lineNum,fp,fw);
                }
                else{

                    fprintf(fw, "float %s;\n", token);
                    strcat(scalars, name);
                    strcat(allVariables, name);
                    while(*c ==' ' && *c){
                        c++;
                    }
                    if(*c=='\n' || *c == '\0'){
                        lineNum++;
                        continue;
                    }
                    else if (*c){
                        error(lineNum,fp,fw);
                    }

                }}
            else{
                error(lineNum,fp,fw);
            }

        }
        //vector defining
        else if (strcmp(token,"vector")==0){
            c+=6;
            q = strcpy(q,c) ;

            token = strtok(q,"]");
            token = strtok(token," ");
            while(*c ==' ' && *c){
                c++;
            }
            c= strchr(c,']');
            c++;
            name = strcpy(name,token);
            name = strtok(name,"[");
            strcat(name,"\n");
            name = strRevCat(name,"\n");
            if (*token){
                if(strstr(allVariables,name)!= NULL){
                    error(lineNum,fp,fw);
                }
                else {

                    fprintf(fw, "float %s][1];\n", token);
                    strcat(token,"][1]\n");
                    strcat(vectors, token);
                    strcat(allVariables, name);
                    strcat(matrices, token);
                    while(*c ==' ' && *c){
                        c++;
                    }
                    if(*c=='\n' || *c == '\0'){
                        lineNum++;
                        continue;
                    }
                    else if (*c){
                        error(lineNum,fp,fw);
                    }
                }
            }
            else{
                error(lineNum,fp,fw);
            }
        }
        //matrix defining 
        else if (strcmp(token,"matrix")==0){
            c+=6;
            q = strcpy(q,c) ;
            token = strtok(q,"\n");
            //token = strtok(token," ");

            q = strcpy(q,token);
            char *left = (char*)calloc(256,1);
            left = myTok(q,',');
            left = strchr(left,'[');
            ///q = strcpy(q,token);
            q = strchr(q,',');
            q++;
            q = strtok(q,"]");
            name = myTok(token,'[');

            name = strtok(name," ");
            left = strRevCat(left,name);
            strcat(name,"\n");
            name = strRevCat(name,  "\n");
            while(*c ==' ' && *c){
                c++;
            }
            c =strchr(c,']');
            c++;
            if (*token){
                if(strstr(allVariables,name)!= NULL){
                    error(lineNum,fp,fw);
                }
                else {
                    strcat(left,"][");
                    strcat(left,q);
                    strcat(left,"]");
                    fprintf(fw, "float %s;\n", left);
                    strcat(left,"\n");
                    strcat(matrices,left);
                    strcat(allVariables,name);
                    while(*c ==' ' && *c){
                        c++;
                    }
                    if(*c=='\n' || *c == '\0'){
                        lineNum++;
                        continue;
                    }
                    else if (*c){
                        error(lineNum,fp,fw);
                    }
                }
            }
            else{
                error(lineNum,fp,fw);
            }
        }
        else {

            q = strcpy(q,token);
            q = strtok(q,"(");

            if(strcmp(q,"print")==0){
                while(*c !='(' && *c){
                    c++;
                }
                c++;
                while(*c ==' ' && *c){
                    c++;
                }
                char * exp = (char*) calloc(256,1);
                exp = strcpy(exp,c);

                exp = strRevTok(exp,')');
                exp = strtok(exp," ");
                exp = expr(exp,lineNum,fw,fp);
                if(exp != NULL) {
                    char *expr1 = (char *) calloc(strlen(exp) + 1, 1);
                    strcpy(expr1, exp);
                    strcat(expr1, "\n");
                    expr1 = strRevCat(expr1,"\n");
                    //calling printScalar 
                    if (strstr(scalars, expr1) != NULL || isNumber(exp)) {
                        fprintf(fw, "printScalar(%s);\n", exp);
                    } else {
                        strcpy(expr1, exp);
                        strcat(expr1, "[");
                        expr1= strRevCat(expr1,"\n");
                        if (strstr(matrices, expr1) != NULL) {
                            char *row = strstr(matrices, expr1);
                            row += (strlen(expr1));
                            char *column = strchr(row, '[');
                            column++;
                            row = myTok(row, ']');
                            column = myTok(column, ']');
                            //calling printMatrix 
                            fprintf(fw, "printMatrix((float*)%s,%s,%s);\n", exp, row, column);
                        } else {
                            error(lineNum,fp,fw);
                        }
                    }
                }
                else{
                    error(lineNum,fp,fw);
                }


            }
            else if(strcmp(q,"printsep")==0){
                while(*c !='(' && *c){
                    c++;
                }
                c++;
                while(*c ==' ' && *c){
                    c++;
                }
                if (*c != ')'){
                    error(lineNum,fp,fw);
                }
                else{
                    fprintf(fw,"printsep();\n");
                }
            }
            else if (strcmp(q,"for")==0){
                int nested = 0;
                while(*c !='(' && *c){
                    c++;
                }
                c++;
                while(*c ==' ' && *c){
                    c++;
                }
                char *id = strdup(c);
                char *id2;
                id = strtok(id,",");
                id = strtok(id," ");
                c += (strlen(id));
                while(*c ==' ' && *c){
                    c++;
                }
                if (*c == 'i'&& *(c+1) == 'n') {
                    c += 2;
                }
                else if(*c == ',') {
                    c += 1;
                    nested = 1;
                    while(*c ==' ' && *c){
                        c++;
                    }
                    id2 = strdup(c);
                    id2 = strtok(id2," ");
                    c += strlen(id2);
                    while(*c ==' ' && *c){
                        c++;
                    }
                    if (*c == 'i'&& *(c+1) == 'n') {
                        c += 2;
                    }
                }
                while(*c ==' ' && *c){
                    c++;
                }

                char *expr1 = strdup(c);
                expr1 = strtok(expr1,":");
                expr1 = strtok(expr1," ");
                c += (strlen(expr1));
                expr1 = expr(expr1,lineNum,fw,fp);
                while((*c ==' ' || *c == ':')&& *c){
                    c++;
                }
                char *expr2 = strdup(c);
                expr2 = strtok(expr2,":");
                expr2 = strtok(expr2," ");
                c += (strlen(expr2));
                expr2 = expr(expr2,lineNum,fw,fp);
                while((*c ==' ' || *c == ':')&& *c){
                    c++;
                }
                char *expr3 = strdup(c);
                if(nested == 0) {
                    expr3 = strtok(expr3, "\n");
                    expr3 = strtok(expr3, " ");
                    expr3 = strRevTok(expr3, ')');
                    c += (strlen(expr3));
                    expr3 = expr(expr3,lineNum,fw,fp);
                    fprintf(fw, "for(%s = %s; %s<=%s; %s += %s){\n", id, expr1, id, expr2, id, expr3);
                    f = 1;
                }
                else{
                    f= 2;
                    expr3 = parseFor(expr3);
                    expr3 = strtok(expr3, " ");
                    c += (strlen(expr3));
                    expr3 = expr(expr3,lineNum,fw,fp);
                    while((*c ==' ' || *c == ',')&& *c){
                        c++;
                    }
                    char *expr4 = strdup(c);
                    expr4 = strtok(expr4,":");
                    expr4 = strtok(expr4," ");
                    c += (strlen(expr4));
                    expr4 = expr(expr4,lineNum,fw,fp);
                    while((*c ==' ' || *c == ':')&& *c){
                        c++;
                    }
                    char *expr5 = strdup(c);
                    expr5 = strtok(expr5,":");
                    expr5 = strtok(expr5," ");
                    c += (strlen(expr5));
                    expr5 = expr(expr5,lineNum,fw,fp);
                    while((*c ==' ' || *c == ':')&& *c){
                        c++;
                    }
                    char *expr6 = strdup(c);
                    expr6 = strRevTok(expr6, ')');
                    expr6 = strtok(expr6, " ");
                    c += (strlen(expr6));
                    //translating for function for c 
                    expr6 = expr(expr6,lineNum,fw,fp);
                    fprintf(fw, "for(%s = %s; %s<=%s; %s += %s){\n"
                                "\tfor(%s = %s; %s<=%s; %s += %s){\n", id, expr1, id, expr2, id, expr3,id2, expr4, id2, expr5, id2, expr6);
                }
            }
            else if(strcmp(myTok(q,'\n'),"}")==0 || strcmp(myTok(q,' '),"}")==0||strcmp(myTok(q,'\0'),"}")==0){
                if(f==1){
                    fprintf(fw,"}\n");
                    f=0;
                }
                else if(f==2){
                    fprintf(fw,"}\n");
                    fprintf(fw,"}\n");
                    f=0;
                }
                else{
                    error(lineNum,fp,fw);
                }

            }
            else{
                q = strtok(q,"=");
                q = strtok(q," ");
                char * temp= strdup(q);
                strcat(q,"\n");
                q = strRevCat(q,"\n");
                if(strstr(allVariables,q)!=NULL){
                    if(strstr(scalars,q)!=NULL){
                        char *exp =  calloc(256,1);
                        strcpy(exp,c);
                        exp = strchr(exp,'=');
                        exp++;
                        strtok(exp,"\n");
                        exp = expr(exp,lineNum,fw,fp);
                        if (exp!=NULL){
                            char* cpy1 = calloc(256,1);
                            cpy1 = strcpy(cpy1,exp);
                            strcat(cpy1,"\n");
                            cpy1 = strRevCat(cpy1,"\n");
                            if(strstr(scalars,cpy1)!=NULL || isNumber(exp)){
                                fprintf(fw,"%s = %s;\n",temp,exp);
                            }
                            else{
                                error(lineNum,fp,fw);
                            }
                        }

                    }
                    else {
                        strcpy(q,temp);
                        strcat(q,"[");
                        q = strRevCat(q,"\n");
                        if(strstr(matrices,q)!=NULL){
                            char *row1 = strstr(matrices, q);
                            row1 = strchr(row1, '[');
                            row1++;
                            char *col1 = strdup(row1);
                            row1 = myTok(row1, ']');
                            col1 = strchr(col1, '[');
                            col1++;
                            col1 = myTok(col1, ']');
                            // checking dimensions and assigning 
                            char *exp = strdup(c);
                            exp = strchr(exp,'=');
                            exp++;
                            if(strstr(exp,"{")!=NULL){
                                exp = strstr(exp,"{");
                                exp++;
                                exp = strRevTok(exp,'}');
                                fprintf(fw,"initMatrix(\"%s\",(float*)%s,%s,%s);\n",exp,temp,row1,col1);
                            }
                            else{
                                strtok(exp, "\n");
                                exp = expr(exp, lineNum, fw, fp);
                                char *cpy = calloc(strlen(exp) + 1, 1);
                                cpy = strcpy(cpy, exp);
                                strcat(cpy, "[");
                                cpy = strRevCat(cpy, "\n");
                                if (strstr(matrices, cpy) != NULL) {

                                    char *row2 = strstr(matrices, cpy);
                                    row2 = strchr(row2, '[');
                                    row2++;
                                    char *col2 = strdup(row2);
                                    row2 = myTok(row2, ']');
                                    col2 = strchr(col2, '[');
                                    col2++;
                                    col2 = myTok(col2, ']');
                                    strtok(row1, " ");
                                    strtok(row2, " ");
                                    strtok(col1, " ");
                                    strtok(col2, " ");
                                    if (strcmp(row1, row2) == 0 && strcmp(col1, col2) == 0) {
                                        fprintf(fw, "assign((float *)%s, (float *) %s,%s, %s);\n", exp, temp, row1,
                                                col1);
                                    } else {
                                        error(lineNum, fp, fw);
                                    }
                                } else {
                                    error(lineNum, fp, fw);
                                }
                            }
                        }
                    }
                }
                else{
                    error(lineNum,fp,fw);
                }
            }
        }
        lineNum++;

    }
    if(f){
        error(lineNum,fp,fw);
    }
    fprintf (fw,"return 0;\n}");
    fclose(fw);
    fclose(fp);
    free(matrices);
    free(vectors);
    free(scalars);
    free(allVariables);


    return(0);
}
