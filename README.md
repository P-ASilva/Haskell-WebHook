# Haskell Webhook

Este projeto é um servidor web em Haskell que recebe notificações de webhook, valida payloads JSON, verifica duplicidade de transações e confirma ou cancela pagamentos via requisições HTTP para os endpoints configurados.


## Pré-requisitos

* [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler) instalado (versão recomendada >= 8.10)
* [Stack](https://docs.haskellstack.org/en/stable/README/) para buildar e gerenciar dependências

## Instalação

1. Clone o repositório:

   ```bash
   git clone https://github.com/P-ASilva/Haskell-WebHook.git
   cd Haskell-WebHook
   ```

2. Compile com Stack:

   ```bash
   stack setup
   stack build
   ```

3. Para limpar o cache e buildar do zero (opcional):

   ```bash
   stack clean
   stack build
   ```

## Rodando o servidor

Para iniciar o servidor localmente na porta padrão (5001):

```bash
stack run
```

O servidor irá expor endpoints HTTP para receber notificações de webhook.

## Testando a aplicação

Você pode testar o webhook enviando requisições POST para o endpoint apropriado, por exemplo:

```bash
curl -X POST http://localhost:5001/webhook \
     -H "Content-Type: application/json" \
     -H "X-Webhook-Token: meu-token-secreto" \
     -d '{"event":"payment_success","transaction_id":"abc123","amount":49.90,"currency":"BRL","timestamp":"2023-10-01T12:00:00Z"}'
```

Exemplos adicionais no arquivo `test_webhook.py`.

## Funcionalidades

* Validação dos payloads JSON recebidos (campos obrigatórios, valores numéricos positivos)
* Validação do token de autenticação do webhook (`X-Webhook-Token`)
* Checagem para evitar transações duplicadas (com mesmo `transaction_id`)
* Confirmação ou cancelamento automático da transação via requisição HTTP local
* Logs simples no console para debug dos payloads recebidos


## Estrutura do projeto

* `Validator.hs`: Validação e controle de transações duplicadas - Atualmente em memória
* `Confirmator.hs`: Envio das confirmações ou cancelamentos para url configurada
* `Webhook.hs`: Servidor web e lógica do endpoint webhook
* `Main.hs`: Roda o servidor. No futuro, deve também chamar a validação do banco de dados/inicialização.

### Modificações Pendentes
- Tornar rotas configuráveis.
- Adicionar Banco de dados para gravar transações.
- Trocar para um formato de requisições mais seguro. (HTTPS)
- Implementar token não-trivial: mecanismo de segurança se resume a um token fixo no momento
- Checar formato de data valido.