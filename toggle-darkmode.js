<script>
document.addEventListener("DOMContentLoaded", function () {
  // Criação do botão
  const btn = document.createElement("button");
  btn.id = "dark-mode-toggle";
  btn.title = "Alternar modo claro/escuro";
  btn.innerText = "🌙/☀️";
  document.body.appendChild(btn);

  // Função de alternância
  btn.addEventListener("click", function () {
    document.body.classList.toggle("dark-mode");
    // Salva preferência
    localStorage.setItem("dark-mode", document.body.classList.contains("dark-mode"));
  });

  // Aplica modo escuro se já estiver salvo
  if (localStorage.getItem("dark-mode") === "true") {
    document.body.classList.add("dark-mode");
  }
});
</script>


