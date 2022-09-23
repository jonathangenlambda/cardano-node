self: system:

let
  inherit (self.inputs.nixpkgs) lib;
  inherit (self.inputs.utils.lib) flattenTree;

  ciInputName = "GitHub event";
in rec {
  tasks = let
    common = {
      config,
      ...
    }: {
      preset = {
        nix.enable = true;
        github-ci = {
          enable = config.actionRun.facts != {};
          repo = "input-output-hk/cardano-node";
          sha = config.preset.github-ci.lib.getRevision ciInputName null;
          clone = false;
        };
      };
    };

    flakeUrl = {
      config,
      lib,
      ...
    }: lib.escapeShellArg (
      if config.actionRun.facts != {}
      then with config.preset.github-ci; "github:${repo}/${sha}"
      else "."
    );

    # the attribute name in `hydraJobs` for the current system
    os = {
      x86_64-linux = "linux";
      x86_64-darwin = "macos";
    }.${system};

    # returns flattened attrset of `hydraJobs.${os}` and system-agnostic hydra jobs
    systemHydraJobs = hydraJobs:
      lib.pipe hydraJobs.${os} [
        (__mapAttrs (_: flattenTree))
        (__mapAttrs (category: lib.mapAttrs' (jobName: lib.nameValuePair "${os}.${category}.${jobName}")))
        __attrValues
        (__foldl' lib.mergeAttrs {})
      ]
      // { inherit (hydraJobs) build-version cardano-deployment; };

    # returns attrset of tullia tasks named with the given prefix
    # that run the corresponding task and depend on each other in the order given
    taskSequence = taskNamePrefix: taskNames: lib.listToAttrs (
      lib.imap0 (i: taskName: lib.nameValuePair
        (taskNamePrefix + taskName)
        ({...}: {
          imports = [tasks.${taskName}];
          after = lib.optional (i > 0) (
            taskNamePrefix + __elemAt taskNames (i - 1)
          );
        })
      ) taskNames
    );

    # returns attrset of tullia tasks that run the given hydra jobs sequentially in lexicographical order
    hydraJobsTaskSequence = taskNamePrefix: hydraJobs: taskSequence taskNamePrefix (__attrNames hydraJobs);

    ciPushTasks = hydraJobsTaskSequence "ci/push/" (systemHydraJobs self.outputs.hydraJobs);
    ciPrTasks = hydraJobsTaskSequence "ci/pr/" (systemHydraJobs self.outputs.hydraJobsPr);
  in
    (__mapAttrs (jobName: _: (args: {
      imports = [common];

      command.text = ''
        job=${flakeUrl args}#hydraJobs.${lib.escapeShellArg jobName}
        echo Building "$job"…
        nix build -L "$job"
      '';

      # some hydra jobs run NixOS tests
      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 8;
      nomad.resources.cpu = 10000;
    })) (systemHydraJobs self.outputs.hydraJobs))
    // ciPushTasks
    // ciPrTasks
    // {
      "ci/push" = {...}: {
        imports = [common];
        after = [(lib.last (__attrNames ciPushTasks))];
      };

      "ci/pr" = {...}: {
        imports = [common];
        after = [(lib.last (__attrNames ciPrTasks))];
      };
    };

  actions = {
    "cardano-node/ci/push" = {
      task = "ci/push";
      io = ''
        #lib.io.github_push
        #input: "${ciInputName}"
        #repo: "input-output-hk/cardano-node"
      '';
    };

    "cardano-node/ci/pr" = {
      task = "ci/pr";
      io = ''
        #lib.io.github_pr
        #input: "${ciInputName}"
        #repo: "input-output-hk/cardano-node"
        #target_default: false
      '';
    };
  };
}
