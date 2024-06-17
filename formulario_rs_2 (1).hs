{-# LANGUAGE RecordWildCards #-}

import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Data.Char (toLower)

data Gender = Male | Female | Other deriving (Show, Read, Enum, Bounded)

data Person = Person
  { nome :: String,
    idade :: Int,
    gender :: Gender,
    cpf :: String,
    profissao :: String,
    endereco :: String,
    dano_geral :: String,
    contato :: String,
    disp_est :: String,
    n_abrigo :: Int,
    rec_aux :: String
  } deriving (Show)

data Abrigo = Abrigo
                      { nomeAbrigo :: String,
                        numero :: Int,
                        responsavel :: String,
                        contatoAbrigo :: String,
                        refeitorio :: String,
                        enderecoAbrigo :: String,
                        n_pessoas :: String,
                        esp_disponivel :: Int,
                        pix_doacao :: String,
                        banheiros :: String
                      } deriving (Show)


main :: IO ()
main = do
  putStrLn "Bem-vindo ao formulário de captura de informações do Desastre do Rio Grande do SUl!"
  person <- capturePersonData
  putStrLn "Informações pessoais capturadas com sucesso:"
  print person
  abrigo <- captureAbrigoData
  putStrLn "Informações do abrigo capturadas com sucesso:"
  print abrigo
   

capturePersonData :: IO Person
capturePersonData = do
  nome <- prompt "Nome Completo: "

  idade <- promptWithValidation "Idade: " validateidade

  gender <- promptWithValidation "Gênero (Male, Female, Other): " validateGender

  cpf <- prompt "CPF: "

  profissao <- prompt"Profissão: "

  endereco <- prompt "Endereço (Rua, Cidade, Estado, País): "

  dano_geral <- prompt "Descrição do Impacto Pessoal: "

  contato <- prompt "Endereço para contato: "

  disp_est <- prompt "Disposição de se estabelecer em outro lugar: "

  n_abrigo <- promptInt "Número do abrigo: "

  rec_aux <- prompt "Auxílios Recebidos: "

  return Person { nome, idade, gender, cpf, profissao, endereco, dano_geral, contato, disp_est, n_abrigo, rec_aux }

captureAbrigoData :: IO Abrigo
captureAbrigoData = do
  nomeAbrigo <- prompt "Nome do abrigo: "

  numero <- promptInt "Número do abrigo: " 

  responsavel <- prompt "Responsável do abrigo: " 

  refeitorio <- prompt "Possui Refeitório: "

  contatoAbrigo <- prompt "Contato do abrigo: "

  enderecoAbrigo <- prompt "Endereço do abrigo: "

  n_pessoas <- prompt "Número de pessoas no abrigo: "

  esp_disponivel <- promptInt "Espaço disponível no abrigo: "

  pix_doacao <- prompt "Pix para doação do abrigo: "

  banheiros <- prompt "Número de banheiros no abrigo: "

  return Abrigo { nomeAbrigo, numero, responsavel, refeitorio, contatoAbrigo, enderecoAbrigo, n_pessoas, esp_disponivel, pix_doacao, banheiros }

prompt :: String -> IO String
prompt text = do
  putStr text
  getLine

promptInt :: String -> IO Int
promptInt text = do
  putStr text
  readLn

promptWithValidation :: String -> (String -> Either String a) -> IO a
promptWithValidation text validate = do
  putStr text
  input <- getLine
  case validate input of
    Left errorMsg -> do
      putStrLn errorMsg
      promptWithValidation text validate
    Right value -> return value

validateidade :: String -> Either String Int
validateidade input =
  case readMaybe input of
    Just idade | idade > 0 -> Right idade
    _ -> Left "Idade inválida! Por favor, insira um número inteiro positivo."

validateGender :: String -> Either String Gender
validateGender input =
  case map toLower input of
    "male" -> Right Male
    "female" -> Right Female
    "other" -> Right Other
    _ -> Left "Gênero inválido! Por favor, insira Male, Female ou Other."

