# No gpu-layers or n-cpu-moe: llama.cpp's `fit` sizes the GPU/CPU
# split from the VRAM free at load time, and setting n-cpu-moe
# turns it off.
{
  qwen3-coder-30b = {
    name = "Qwen3 Coder 30B A3B";
    # Sampling defaults are embedded in the GGUF.
    settings.hf-repo = "ggml-org/Qwen3-Coder-30B-A3B-Instruct-GGUF:Q4_K_M";
  };
  qwen3-14b = {
    name = "Qwen3 14B";
    settings = {
      hf-repo = "unsloth/Qwen3-14B-GGUF:IQ4_XS";
      reasoning = "off";
      temp = "0.7";
      top-p = "0.8";
      top-k = 20;
      # The model card asks for 0; llama.cpp's own default is 0.05.
      min-p = "0";
    };
  };
  gpt-oss-20b = {
    name = "gpt-oss 20B";
    settings = {
      hf-repo = "ggml-org/gpt-oss-20b-GGUF:MXFP4";
      # gpt-oss cannot turn reasoning off; low is its floor.
      reasoning-effort = "low";
      temp = "1.0";
      top-p = "1.0";
    };
  };
  gemma4-26b = {
    name = "Gemma 4 26B A4B";
    settings = {
      hf-repo = "ggml-org/gemma-4-26B-A4B-it-GGUF:Q4_0";
      # Skips the vision projector, about 1 GiB of VRAM.
      no-mmproj = true;
      reasoning = "off";
    };
  };
  "qwen3.5-4b" = {
    name = "Qwen3.5 4B";
    # The model card's figure for most queries; thinking outruns 8192.
    maxTokens = 32768;
    settings = {
      hf-repo = "unsloth/Qwen3.5-4B-GGUF:Q8_0";
      # The model card asks for at least 128K to keep thinking intact.
      ctx-size = 131072;
      # The card's thinking-mode set for general tasks.
      temp = "1.0";
      top-p = "0.95";
      top-k = 20;
      min-p = "0";
      presence-penalty = "1.5";
    };
  };
  "granite-4.2-3b" = {
    name = "Granite 4.2 3B";
    settings = {
      hf-repo = "ibm-granite/granite-4.2-3b-GGUF:Q8_0";
      # The native 128K would take about 14 GiB of the 16.
      ctx-size = 65536;
      # The model card asks for these in every mode, tool calling included.
      temp = "1.0";
      top-p = "0.95";
    };
  };
}
