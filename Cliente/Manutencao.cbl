       identification division.
       program-id. Manutencao.

       environment division.
       Input-Output Section.
       File-Control.
           copy "sel-sisfile.cpy".

       data division.
       File Section. 
           copy "fd-sisfile.cpy".
       
       working-storage section.
       77 ws-indice        pic 9(03) value zeros.
       01 ws-file-status.
           03 ws-file-1    pic x(01).
           03 ws-file-2    pic x(01) usage display.
           
       linkage section.
       01 lnk-operacao     pic x(01).
           78 lnk-op-cadastrar value '1'.
           78 lnk-op-consultar value '2'.
           78 lnk-op-alterar   value '3'.
       01 lnk-dados.
           10  lnk-codigo              pic 9(03).
           10  lnk-nome                pic x(30).
           10  lnk-endereco            pic X(40).
           10  lnk-cidade              pic X(20).
           10  lnk-estado              pic X(02).
           10  lnk-sexo                pic X(09).
       01 lnk-mensagem     pic x(60). 
           
       procedure division using lnk-operacao
                                lnk-dados
                                lnk-mensagem.

       0001-Operacao.
           evaluate lnk-operacao
               when '1'
                   perform 0100-Cadastrar
               when '2'
                   perform 0200-Consultar
               when '3'
                   perform 0300-Alterar
               when '4'
                   perform 0400-Listar
           end-evaluate
           go to 9999-Sair.
       
       0100-Cadastrar.
           open i-o sis-file
           move 999 to sis-codigo
           start sis-file key is not greater then sis-codigo
           read sis-file next
           add 1 to sis-codigo
           if sis-codigo equal zeros
               move 1 to sis-codigo
           end-if
           move sis-codigo to lnk-dados(1:3)
           move lnk-dados to sis-dados
           write sis-dados
           if ws-file-1 equal '0'
               string "Registro Cadastrado com Sucesso!"
               delimited by size " Codigo : "
               delimited by size sis-codigo 
               delimited by size into lnk-mensagem
           else
               string "Registro não Cadastrado. Erro: "
               delimited by size ws-file-status
               delimited by size into lnk-mensagem
           end-if.

       0200-Consultar.
           open input sis-file
           move lnk-dados to sis-dados
           read sis-file key is sis-codigo
           if ws-file-status equal '23'
               string "Registro não Encontrado!"
               delimited by size into lnk-mensagem
           else
               move sis-dados to lnk-dados
           end-if.

       0300-Alterar.
           open i-o sis-file
           move lnk-dados to sis-dados
           read sis-file key is sis-codigo
           move lnk-dados to sis-dados
           rewrite sis-dados
           if ws-file-1 equal '0'
               string "Registro Alterado com Sucesso!"
               delimited by size " Codigo : "
               delimited by size sis-codigo 
               delimited by size into lnk-mensagem
           else
               string "Não foi possível alterar o Registro. Erro: "
               delimited by size ws-file-status
               delimited by size into lnk-mensagem
           end-if.

       0400-Listar.
           open input sis-file
           move lnk-dados to sis-dados
           read sis-file
           if ws-file-1 not equal '0'
               read sis-file next
           end-if
           move sis-dados to lnk-dados
           move ws-file-status to lnk-mensagem.

       9999-Sair.
           close sis-file.

       end program Manutencao.
