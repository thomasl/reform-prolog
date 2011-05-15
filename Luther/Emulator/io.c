/*    File:	 io.c  (~jb/Reform/Luther/Emulator/io.c)
 *    Author:	 Johan Bevemyr
 *    Created:	 Sun Nov  6 17:21:51 1994
 *    Purpose:   New IO interface. This one is builtin on buffered 
 *               read/write.
 */ 


 /* 
    The following interface functions are provided:

    char *luther_fgetc(STREAM *)
    void luther_fputc(char, STREAM *)
    void luther_fputs(char *, STREAM *)

    BOOL luther_fopen(char *, MODE)
    BOOL luther_fclose(STREAM *)
       

    a STREAM is described by the following stucture:

    typedef struct {
       char buffer[STREAM_BUF_SIZE];
       it 
       
 */

       
