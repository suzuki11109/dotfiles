# reuse "kubectl" completions on "kubecolor"
function kubecolor --wraps kubectl
  command kubecolor $argv
end

# completions for k
function k --wraps kubectl
  command kubectl $argv
end

# completions for kubectl
kubectl completion fish | source
