/***********************************************************************************/
/* Eduardo Acuña Yeomans 2015                                                      */
/*                                                                                 */
/* Programa para escanear un archivo de texto y enlistar el contenido considerando */
/* a los espacios en blanco como delimitadores.                                    */
/***********************************************************************************/

#include <stdio.h>
#include <ctype.h>

/* regresa el caracter al inicio de un puerto sin consumirlo */
int peekc(FILE* file_ptr)
{
  int c = fgetc(file_ptr);
  ungetc(c, file_ptr);
  return c;
}

/* consume el espacio en blanco contiguo del inicio del puerto */
void consume_whitespace(FILE* file_ptr)
{
  int c;
  while(!feof(file_ptr) && isspace(c = peekc(file_ptr)))
    getc(file_ptr);
}

/* realiza el escaneo del contenido de el archivo de entrada y coloca en el puerto de
   salida el resultado */
void scan(FILE* input_file_ptr, FILE* output_file_ptr)
{
  int c;
  while(!feof(input_file_ptr)){
    c = peekc(input_file_ptr);
    if(isspace(c)){
      consume_whitespace(input_file_ptr);
      putc('\n', output_file_ptr);
    }else
      putc(getc(input_file_ptr), output_file_ptr);
  }
}

int main(int argc, char* argv[])
{
  /* Solo acepta un argumento de entrada */
  if(argc == 1 || argc > 2){
    puts("You need to provide just one file name.");
    return 1;
  }

  printf("File name provided \"%s\".\n", argv[1]);

  FILE* file_ptr;
  file_ptr = fopen(argv[1], "r");

  scan(file_ptr, stdout);

  fclose(file_ptr);

  return 0;
}
