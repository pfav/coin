  <script>
    import Transaction from './Transaction.svelte';
    
    export let block;
  </script>
  
  <div class='row'>
    <div class='col-sm-12'>
      <table class="horizontal">
        <thead>
          <tr>
            <th>Version</th>
            <th>Index</th>
            <th>Hash</th>
            <th>Prev Hash</th>
            <th>Timestamp</th>
            <th>Difficulty</th>
            <th>Nonce</th>
            <th>Transactions</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td data-label="Version">{ block.version }</td>
            <td data-label="Index">{ block.index }</td>
            <td data-label="Hash"><code>{ block.hash }</code></td>
            <td data-label="Prev Hash">
              
              {#if block.index != 0 }
              <a href="#/block/{ block["prev-hash"] }"> { block['prev-hash'] }</a>
              {:else}
              <code>{ block['prev-hash'] }</code>
              {/if}
            </td>
            <td data-label="Timestamp">{ block.timestamp } { (new Date(block.timestamp).toISOString()) }</td>
            <td data-label="Difficulty">{ block.difficulty }</td>
            <td data-label="Nonce">{ block.nonce }</td>
            <td data-label="Transactions">{ block.transactions.length }</td>
          </tr>
        </tbody>
      </table>
      
      {#each block.transactions as tx, i (tx.hash)}
      <div>
        <Transaction idx={i} {tx} />
      </div>                
      {/each}
    </div>
  </div>
  
  
