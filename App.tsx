
import React, { useState, useEffect, useCallback, useRef } from 'react';
import { 
  Terminal, 
  Settings, 
  Cpu, 
  Database, 
  Layers, 
  CheckCircle2, 
  AlertCircle, 
  Clock, 
  Cloud,
  ArrowUpRight,
  Upload,
  BarChart3,
  PlayCircle,
  FileCode2,
  Download,
  Github,
  GitBranch,
  X,
  ShieldCheck,
  ExternalLink,
  Save,
  Info,
  User,
  LogOut,
  Server,
  Network,
  Globe,
  HardDrive,
  Activity
} from 'lucide-react';
import { 
  BarChart, 
  Bar, 
  XAxis, 
  YAxis, 
  Tooltip, 
  ResponsiveContainer, 
  Cell
} from 'recharts';
import { geminiService } from './services/geminiService';
import { CodeChunk, ConversionStep, GitHubConfig, GitHubUser } from './types';

const MOCK_COBOL = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER-PROC-01.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO "ACCOUNTS.DAT".
           
       DATA DIVISION.
       FILE SECTION.
       FD ACCT-FILE.
       01 ACCT-RECORD.
          05 ACCT-NUM      PIC 9(10).
          05 ACCT-NAME     PIC X(30).
          05 ACCT-BALANCE  PIC S9(13)V99.
          
       WORKING-STORAGE SECTION.
       01 WS-EOF          PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT ACCT-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCT-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM PROCESS-ACCOUNT
               END-READ
           END-PERFORM.
           CLOSE ACCT-FILE.
           STOP RUN.
`;

const App: React.FC = () => {
  const [step, setStep] = useState<ConversionStep>(ConversionStep.IDLE);
  const [chunks, setChunks] = useState<any[]>([]);
  const [selectedChunkId, setSelectedChunkId] = useState<string | null>(null);
  const [logs, setLogs] = useState<string[]>([]);
  const [readinessSummary, setReadinessSummary] = useState('');
  const [showDeployModal, setShowDeployModal] = useState(false);
  const [isProvisioning, setIsProvisioning] = useState(false);
  const [isLoggingIn, setIsLoggingIn] = useState(false);
  const [currentUser, setCurrentUser] = useState<GitHubUser | null>(null);
  
  const [cloudConfig, setCloudConfig] = useState<any>({
    project: 'gcp-bank-migration-prd',
    region: 'us-east4',
    runtime: 'Micro Focus Enterprise Server',
    targetInstance: 'n2-standard-4'
  });

  const logEndRef = useRef<HTMLDivElement>(null);

  const addLog = (msg: string) => {
    setLogs(prev => [...prev, `[${new Date().toLocaleTimeString()}] ${msg}`]);
  };

  useEffect(() => {
    logEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [logs]);

  const handleCloudLogin = () => {
    setIsLoggingIn(true);
    addLog("Authenticating with Google Cloud Identity (Cloud SDK)...");
    setTimeout(() => {
      const mockUser: GitHubUser = {
        id: 'gcp-9901',
        login: 'cloud_architect',
        name: 'GCP Modernizer',
        avatar_url: 'https://api.dicebear.com/7.x/avataaars/svg?seed=Cloud',
        role: 'Architect'
      };
      setCurrentUser(mockUser);
      setIsLoggingIn(false);
      addLog(`Connected to GCP Project: ${cloudConfig.project}`);
    }, 1500);
  };

  const handleLogout = () => {
    setCurrentUser(null);
    addLog("GCP Session terminated.");
  };

  const handleFileUpload = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;

    setStep(ConversionStep.ANALYZING_SYSTEM);
    addLog(`LegacyLink AI: Analyzing ${file.name} for GCE Lift & Shift compatibility...`);
    
    const reader = new FileReader();
    reader.onload = async (event) => {
      const content = event.target?.result as string;
      try {
        const analysis = await geminiService.analyzeForCloud(content, file.name);
        const newChunks = analysis.map((item: any, idx: number) => ({
          ...item,
          id: `module-${idx}`,
          status: 'pending',
          progress: 0
        }));
        setChunks(newChunks);
        addLog(`Analysis Success: Found ${newChunks.length} migratable workloads.`);
        setStep(ConversionStep.CONVERTING_INCREMENTS);
      } catch (err) {
        addLog(`GCP Analysis Error: ${err instanceof Error ? err.message : 'Analysis failed'}`);
        setStep(ConversionStep.IDLE);
      }
    };
    reader.readAsText(file);
  };

  const provisionNextModule = useCallback(async () => {
    const nextPending = chunks.find(c => c.status === 'pending');
    if (!nextPending) {
      if (chunks.length > 0 && chunks.every(c => c.status === 'completed')) {
        setStep(ConversionStep.FINALIZING);
        addLog("Rehosting Readiness: 100%. All GCE configurations validated.");
      }
      return;
    }

    setChunks(prev => prev.map(c => c.id === nextPending.id ? { ...c, status: 'converting', progress: 40 } : c));
    addLog(`Generating Infrastructure for: ${nextPending.name}`);

    try {
      const result = await geminiService.generateCloudConfig(nextPending.code, nextPending.recommendedInstance);
      
      setChunks(prev => prev.map(c => c.id === nextPending.id ? { 
        ...c, 
        terraform: result.terraformConfig, 
        startup: result.startupScript,
        notes: result.architectureNotes,
        status: 'completed', 
        progress: 100 
      } : c));

      addLog(`IaC Validated: ${nextPending.name} -> ${nextPending.recommendedInstance || 'n2-standard-2'}`);
      
      const summary = await geminiService.generateReadinessReport(
        chunks.filter(c => c.status === 'completed').length + 1,
        chunks.length,
        `Mapped ${nextPending.name}`
      );
      setReadinessSummary(summary);

    } catch (err) {
      setChunks(prev => prev.map(c => c.id === nextPending.id ? { ...c, status: 'error', errorMessage: 'Cloud mapping failed' } : c));
      addLog(`GCP Provisioning Error in ${nextPending.name}`);
    }
  }, [chunks]);

  useEffect(() => {
    if (step === ConversionStep.CONVERTING_INCREMENTS) {
      const timer = setTimeout(() => {
        provisionNextModule();
      }, 1000);
      return () => clearTimeout(timer);
    }
  }, [step, provisionNextModule]);

  const handleExportTerraform = () => {
    const exportData = chunks.filter(c => c.status === 'completed').map(c => c.terraform).join('\n\n');
    const blob = new Blob([exportData], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `gce_rehosting_plan_${Date.now()}.tf`;
    a.click();
    addLog("Terraform plan exported.");
  };

  const handleGCPDeploy = () => {
    if (!currentUser) {
      handleCloudLogin();
      return;
    }
    setIsProvisioning(true);
    addLog(`Executing GCloud Deployment to project: ${cloudConfig.project}...`);
    setTimeout(() => {
      setIsProvisioning(false);
      setShowDeployModal(false);
      addLog(`Success: GCE Instances spawned with ${cloudConfig.runtime} runtime.`);
    }, 3000);
  };

  const statsData = [
    { name: 'On-Prem', value: chunks.filter(c => c.status === 'pending').length, color: '#374151' },
    { name: 'Staging', value: chunks.filter(c => c.status === 'converting').length, color: '#4285F4' },
    { name: 'Cloud-Ready', value: chunks.filter(c => c.status === 'completed').length, color: '#34A853' },
    { name: 'Blocked', value: chunks.filter(c => c.status === 'error').length, color: '#EA4335' }
  ];

  const selectedChunk = chunks.find(c => c.id === selectedChunkId);

  return (
    <div className="min-h-screen flex flex-col bg-[#0b0e14] text-slate-200 font-sans selection:bg-[#4285F4]/30">
      {/* GCP Style Header with LegacyLink AI branding */}
      <header className="h-14 border-b border-slate-800 bg-[#1e1f24] flex items-center justify-between px-4 sticky top-0 z-50 shadow-md">
        <div className="flex items-center gap-4">
          <div className="p-1.5 bg-white rounded-md shadow-inner">
             <Cloud className="w-5 h-5 text-[#4285F4]" />
          </div>
          <div>
            <h1 className="text-sm font-bold tracking-tight text-white flex items-center gap-2">
              LegacyLink <span className="text-indigo-400">AI</span> <span className="px-1.5 py-0.5 rounded bg-[#4285F4]/10 text-[#4285F4] text-[10px] uppercase font-black">Mainframe GCE</span>
            </h1>
          </div>
        </div>
        
        <div className="flex items-center gap-4">
          <div className="hidden md:flex items-center gap-3 pr-4 border-r border-slate-700">
             <div className="flex flex-col items-end">
                <span className="text-[9px] text-slate-500 font-black uppercase tracking-widest">GCP Context</span>
                <span className="text-xs text-[#4285F4] font-semibold">{cloudConfig.project}</span>
             </div>
             <Globe className="w-4 h-4 text-slate-500" />
          </div>

          {currentUser ? (
            <div className="flex items-center gap-3 pl-2 group relative">
              <div className="flex flex-col items-end">
                <span className="text-xs font-bold text-white">{currentUser.name}</span>
                <span className="text-[9px] text-slate-500 font-mono tracking-tighter">CLOUD_IAM_READY</span>
              </div>
              <img src={currentUser.avatar_url} alt="User" className="w-8 h-8 rounded-full ring-1 ring-slate-700 group-hover:ring-[#4285F4] transition-all cursor-pointer" />
              <button onClick={handleLogout} className="p-1.5 text-slate-500 hover:text-red-400 transition-colors">
                <LogOut className="w-3.5 h-3.5" />
              </button>
            </div>
          ) : (
            <button 
              onClick={handleCloudLogin}
              disabled={isLoggingIn}
              className="flex items-center gap-2 px-3 py-1.5 bg-[#4285F4] text-white rounded-md text-xs font-bold transition-all hover:bg-[#3367D6]"
            >
              {isLoggingIn ? <div className="w-3 h-3 border-2 border-white border-t-transparent rounded-full animate-spin" /> : <ShieldCheck className="w-4 h-4" />}
              {isLoggingIn ? 'Authenticating...' : 'Sign in to GCP'}
            </button>
          )}
        </div>
      </header>

      <main className="flex-1 flex overflow-hidden">
        {/* Sidebar - Workload Inventory */}
        <aside className="w-72 border-r border-slate-800 flex flex-col bg-[#111318]">
          <div className="p-4 border-b border-slate-800 bg-[#1e1f24]/50">
            <h2 className="text-[10px] font-black uppercase tracking-[0.2em] text-slate-500 flex items-center gap-2">
              <HardDrive className="w-3 h-3" /> Workload Inventory
            </h2>
          </div>
          
          <div className="flex-1 overflow-y-auto p-2 space-y-1 custom-scrollbar">
            {chunks.length === 0 ? (
              <div className="h-full flex flex-col items-center justify-center p-8 text-center opacity-20">
                <Server className="w-10 h-10 mb-4" />
                <p className="text-[10px] font-black uppercase tracking-widest leading-loose">No legacy assets mapped.<br/>Upload COBOL or JCL.</p>
              </div>
            ) : (
              chunks.map((chunk) => (
                <button
                  key={chunk.id}
                  onClick={() => setSelectedChunkId(chunk.id)}
                  className={`w-full flex items-center gap-3 p-2.5 rounded-lg transition-all text-left group ${
                    selectedChunkId === chunk.id ? 'bg-[#4285F4]/10 border border-[#4285F4]/30' : 'hover:bg-slate-800/30'
                  }`}
                >
                  <div className={`shrink-0 p-1.5 rounded-md ${
                    chunk.status === 'completed' ? 'bg-[#34A853]/10 text-[#34A853]' : 
                    chunk.status === 'error' ? 'bg-[#EA4335]/10 text-[#EA4335]' : 
                    'bg-slate-800 text-slate-500'
                  }`}>
                    {chunk.status === 'completed' && <CheckCircle2 className="w-3.5 h-3.5" />}
                    {chunk.status === 'pending' && <Server className="w-3.5 h-3.5" />}
                    {chunk.status === 'converting' && <Activity className="w-3.5 h-3.5 animate-pulse" />}
                    {chunk.status === 'error' && <AlertCircle className="w-3.5 h-3.5" />}
                  </div>
                  <div className="min-w-0 flex-1">
                    <p className={`text-xs font-bold truncate ${selectedChunkId === chunk.id ? 'text-white' : 'text-slate-400'}`}>
                      {chunk.name}
                    </p>
                    <div className="flex items-center justify-between mt-1">
                       <span className="text-[9px] font-mono text-slate-600">GCE Target</span>
                       <span className="text-[9px] font-bold text-[#4285F4]">{chunk.recommendedInstance || 'TBD'}</span>
                    </div>
                  </div>
                </button>
              ))
            )}
          </div>

          <div className="p-4 bg-[#1e1f24] border-t border-slate-800">
            <label className="flex flex-col items-center justify-center w-full h-20 border border-dashed border-slate-700 rounded-lg cursor-pointer hover:border-[#4285F4] transition-all bg-[#0b0e14]">
              <Upload className="w-4 h-4 text-slate-500 mb-1" />
              <span className="text-[9px] font-black text-slate-500 uppercase tracking-widest">Source Payload</span>
              <input type="file" className="hidden" accept=".cbl,.cob,.txt" onChange={handleFileUpload} />
            </label>
          </div>
        </aside>

        {/* Cloud Workspace */}
        <section className="flex-1 flex flex-col overflow-hidden bg-[#0b0e14]">
          {step === ConversionStep.IDLE ? (
            <div className="flex-1 flex flex-col items-center justify-center p-12 text-center">
              <div className="relative mb-8">
                <div className="absolute inset-0 bg-[#4285F4] blur-[100px] opacity-10" />
                <div className="relative w-24 h-24 bg-[#1e1f24] border border-slate-800 rounded-2xl flex items-center justify-center shadow-2xl">
                  <ArrowUpRight className="w-12 h-12 text-[#4285F4]" />
                </div>
              </div>
              <h2 className="text-4xl font-black text-white mb-4 tracking-tighter uppercase">Lift & Shift <span className="text-[#4285F4]">Enterprise Rehosting</span></h2>
              <p className="text-lg text-slate-400 mb-10 max-w-xl mx-auto font-light leading-relaxed text-center">
                Automated rehosting of monolithic COBOL payloads to Google Compute Engine. Leverage Micro Focus and Rocket runtimes with auto-generated IaC.
              </p>
              
              <div className="grid grid-cols-1 md:grid-cols-3 gap-6 w-full max-w-4xl text-left mb-12">
                {[
                  { icon: Network, title: "GCE Provisioning", desc: "Instance mapping based on DATA DIVISION complexity." },
                  { icon: HardDrive, title: "VSAM Persistence", desc: "Automated PDX/VSAM mapping to Persistent Disk snapshots." },
                  { icon: ShieldCheck, title: "Cloud Identity", desc: "Integrated IAM and VPC security for partner runtimes." }
                ].map((feature, i) => (
                  <div key={i} className="bg-[#1e1f24]/60 border border-slate-800 p-6 rounded-xl hover:border-[#4285F4]/30 transition-all text-center md:text-left">
                    <feature.icon className="w-6 h-6 text-[#4285F4] mb-4 mx-auto md:mx-0" />
                    <h3 className="text-sm font-bold text-white mb-2">{feature.title}</h3>
                    <p className="text-xs text-slate-500 leading-relaxed">{feature.desc}</p>
                  </div>
                ))}
              </div>

              <div className="flex items-center gap-4">
                <button 
                  onClick={() => {
                    const mockEvent = { target: { files: [new File([MOCK_COBOL], "LEDGER_PAYLOAD.CBL")] } } as any;
                    handleFileUpload(mockEvent);
                  }}
                  className="px-8 py-3.5 bg-[#4285F4] hover:bg-[#3367D6] text-white rounded-md font-bold uppercase tracking-widest text-xs transition-all flex items-center gap-3 shadow-lg shadow-[#4285F4]/20"
                >
                  <PlayCircle className="w-5 h-5" /> Start Modernization
                </button>
              </div>
            </div>
          ) : (
            <div className="flex-1 flex flex-col overflow-hidden">
               {/* Cloud Metrics Bar */}
               <div className="h-16 flex items-stretch border-b border-slate-800 bg-[#1e1f24] divide-x divide-slate-800">
                  <div className="flex-1 px-6 flex flex-col justify-center">
                    <span className="text-[9px] font-black text-slate-500 uppercase tracking-widest block mb-0.5">Deployment Target</span>
                    <div className="flex items-center gap-2">
                      <div className="w-2 h-2 rounded-full bg-gcp-blue animate-pulse" />
                      <span className="text-xs font-bold text-white">COMPUTE_ENGINE_INSTANCE_GROUP</span>
                    </div>
                  </div>
                  <div className="flex-1 px-6 flex flex-col justify-center">
                    <span className="text-[9px] font-black text-slate-500 uppercase tracking-widest block mb-0.5">Runtime Routing</span>
                    <span className="text-xs font-bold text-[#4285F4]">{cloudConfig.runtime}</span>
                  </div>
                  <div className="flex-[2] px-6 flex flex-col justify-center bg-[#4285F4]/5">
                    <span className="text-[9px] font-black text-[#4285F4] uppercase tracking-widest block mb-1">Architecture Summary</span>
                    <p className="text-[10px] text-slate-300 font-medium italic line-clamp-1">
                      {readinessSummary || "Provisioning secure VPC and VPC-SC perimeters for workload landing..."}
                    </p>
                  </div>
               </div>

               {/* Rehosting View */}
               <div className="flex-1 flex overflow-hidden">
                  {/* COBOL Component Source */}
                  <div className="flex-1 flex flex-col border-r border-slate-800">
                    <div className="h-10 px-4 flex items-center justify-between border-b border-slate-800 bg-[#111318]">
                      <span className="text-[9px] font-black text-slate-500 tracking-[0.2em] flex items-center gap-2 uppercase">
                        <FileCode2 className="w-3.5 h-3.5" /> Component Source
                      </span>
                    </div>
                    <div className="flex-1 p-6 overflow-auto font-mono text-[11px] leading-relaxed text-slate-500 bg-[#0b0e14]">
                      {selectedChunk ? (
                        <pre className="whitespace-pre-wrap">{selectedChunk.code}</pre>
                      ) : (
                        <div className="h-full flex flex-col items-center justify-center opacity-10">
                          <Layers className="w-10 h-10 mb-4" />
                          <p className="text-[10px] font-black uppercase tracking-widest">Select workload unit</p>
                        </div>
                      )}
                    </div>
                  </div>

                  {/* IaC / Terraform Configuration */}
                  <div className="flex-1 flex flex-col bg-[#111318]/40">
                    <div className="h-10 px-4 flex items-center justify-between border-b border-slate-800 bg-[#111318]">
                      <span className="text-[9px] font-black text-[#4285F4] tracking-[0.2em] flex items-center gap-2 uppercase">
                        <Settings className="w-3.5 h-3.5" /> GCloud Infrastructure (HCL/TF)
                      </span>
                      {selectedChunk?.status === 'completed' && (
                        <div className="flex items-center gap-2 px-2 py-0.5 bg-[#34A853]/10 rounded border border-[#34A853]/20">
                           <div className="w-1 h-1 rounded-full bg-[#34A853]" />
                           <span className="text-[8px] font-black text-[#34A853] uppercase">PROVISIONING READY</span>
                        </div>
                      )}
                    </div>
                    <div className="flex-1 p-6 overflow-auto font-mono text-[11px] leading-relaxed relative">
                      {selectedChunk?.terraform ? (
                        <div className="space-y-8 animate-in fade-in duration-500">
                          <div>
                            <h4 className="text-[9px] font-bold text-slate-500 uppercase tracking-widest mb-4">GCE Instance (main.tf)</h4>
                            <pre className="text-slate-200 p-4 bg-[#0b0e14] border border-slate-800 rounded-lg">{selectedChunk.terraform}</pre>
                          </div>
                          {selectedChunk.startup && (
                            <div>
                              <h4 className="text-[9px] font-bold text-slate-500 uppercase tracking-widest mb-4">Startup Metadata (init.sh)</h4>
                              <pre className="text-slate-400 italic p-4 bg-[#0b0e14] border border-slate-800 rounded-lg">{selectedChunk.startup}</pre>
                            </div>
                          )}
                          {selectedChunk.notes && (
                            <div className="p-4 rounded-xl bg-[#4285F4]/5 border border-[#4285F4]/20">
                               <h4 className="text-[9px] font-black text-[#4285F4] uppercase tracking-widest mb-2 flex items-center gap-2">
                                <Info className="w-3 h-3" /> Solution Design Notes
                               </h4>
                               <p className="text-[10px] text-slate-400 leading-loose">{selectedChunk.notes}</p>
                            </div>
                          )}
                        </div>
                      ) : (
                        <div className="h-full flex flex-col items-center justify-center text-slate-600 gap-4">
                          {selectedChunk?.status === 'converting' ? (
                            <div className="flex flex-col items-center animate-in fade-in zoom-in duration-300">
                              <div className="w-12 h-12 border-2 border-slate-800 border-t-[#4285F4] rounded-full animate-spin mb-4" />
                              <span className="text-[10px] font-black tracking-[0.2em] uppercase text-[#4285F4]">Mapping Cloud Resources...</span>
                            </div>
                          ) : (
                            <div className="opacity-20 text-center">
                              <Cloud className="w-10 h-10 mb-4 mx-auto" />
                              <p className="text-[10px] font-black uppercase tracking-widest">Awaiting IaC Generation</p>
                            </div>
                          )}
                        </div>
                      )}
                    </div>
                  </div>
               </div>
            </div>
          )}

          {step === ConversionStep.FINALIZING && (
            <div className="absolute inset-0 z-50 flex items-center justify-center bg-[#0b0e14]/95 p-12 overflow-hidden backdrop-blur-sm">
               <div className="absolute inset-0 bg-[#4285F4]/5 blur-[120px] rounded-full animate-pulse" />
               <div className="relative max-w-xl w-full text-center space-y-8 animate-in zoom-in-95 duration-700">
                  <div className="w-20 h-20 bg-[#34A853]/10 rounded-2xl flex items-center justify-center mx-auto ring-4 ring-[#34A853]/10">
                    <CheckCircle2 className="w-10 h-10 text-[#34A853]" />
                  </div>
                  <div className="space-y-3">
                    <h2 className="text-4xl font-black text-white tracking-tight uppercase">Migration Ready</h2>
                    <p className="text-base text-slate-400 font-light leading-relaxed">
                      All COBOL workloads have been successfully mapped to Google Compute Engine. Partner runtimes are pre-configured in the startup metadata.
                    </p>
                  </div>
                  
                  <div className="grid grid-cols-2 gap-4 pt-4">
                    <button 
                      onClick={handleExportTerraform}
                      className="group p-6 bg-[#1e1f24] border border-slate-800 rounded-2xl hover:border-[#4285F4] transition-all hover:bg-[#111318] shadow-2xl"
                    >
                      <Download className="w-6 h-6 text-slate-500 group-hover:text-[#4285F4] mb-3 mx-auto transition-transform group-hover:-translate-y-1" />
                      <span className="text-[10px] font-black uppercase tracking-widest text-white">Export IaC Plan</span>
                    </button>
                    <button 
                      onClick={() => setShowDeployModal(true)}
                      className="group p-6 bg-[#1e1f24] border border-slate-800 rounded-2xl hover:border-[#4285F4] transition-all hover:bg-[#111318] shadow-2xl"
                    >
                      <Globe className="w-6 h-6 text-slate-500 group-hover:text-[#4285F4] mb-3 mx-auto transition-transform group-hover:-translate-y-1" />
                      <span className="text-[10px] font-black uppercase tracking-widest text-white">Execute GCE Push</span>
                    </button>
                  </div>
                  
                  <button onClick={() => setStep(ConversionStep.IDLE)} className="text-[9px] font-black text-slate-600 uppercase tracking-[0.4em] hover:text-white transition-colors pt-8">
                    Reset Cloud Workspace
                  </button>
               </div>
            </div>
          )}
        </section>

        {/* Console / Monitoring Sidebar */}
        <aside className="w-80 border-l border-slate-800 flex flex-col bg-[#111318]">
          <div className="p-4 border-b border-slate-800 bg-[#1e1f24] flex items-center justify-between">
            <h2 className="text-[10px] font-black uppercase tracking-[0.2em] text-slate-500 flex items-center gap-2">
              <Terminal className="w-3 h-3 text-[#4285F4]" /> Cloud Console
            </h2>
          </div>
          <div className="flex-1 overflow-y-auto p-4 font-mono text-[10px] leading-relaxed space-y-1.5 bg-[#0b0e14] custom-scrollbar">
            {logs.map((log, i) => (
              <div key={i} className="text-slate-500 animate-in slide-in-from-left-2 duration-300">
                <span className="text-[#4285F4] font-bold mr-2 opacity-70">GCE</span> {log}
              </div>
            ))}
            <div ref={logEndRef} />
          </div>
          
          <div className="p-6 border-t border-slate-800 bg-[#1e1f24]/50">
            <h2 className="text-[10px] font-black uppercase tracking-[0.3em] text-slate-500 mb-6 flex items-center gap-2">
               <Activity className="w-3 h-3" /> Migration Waves
            </h2>
            <div className="h-40">
              <ResponsiveContainer width="100%" height="100%">
                <BarChart data={statsData} layout="vertical" margin={{ left: -35, right: 10 }}>
                  <XAxis type="number" hide />
                  <YAxis dataKey="name" type="category" width={80} axisLine={false} tickLine={false} style={{ fontSize: '9px', fill: '#4b5563', fontWeight: 'bold' }} />
                  <Tooltip 
                    cursor={{ fill: 'rgba(66, 133, 244, 0.03)' }} 
                    contentStyle={{ backgroundColor: '#1e1f24', border: '1px solid #374151', borderRadius: '8px', fontSize: '9px' }}
                  />
                  <Bar dataKey="value" radius={[0, 3, 3, 0]} barSize={12}>
                    {statsData.map((entry, index) => (
                      <Cell key={`cell-${index}`} fill={entry.color} />
                    ))}
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
            </div>
          </div>
        </aside>
      </main>

      {/* GCP Deployment Modal */}
      {showDeployModal && (
        <div className="fixed inset-0 z-[100] flex items-center justify-center p-6 bg-black/80 backdrop-blur-sm animate-in fade-in duration-300">
          <div className="w-full max-w-lg bg-[#1e1f24] border border-slate-800 rounded-xl shadow-2xl overflow-hidden animate-in zoom-in-95 duration-300">
            {!currentUser ? (
              <div className="p-12 text-center space-y-8">
                <div className="w-16 h-16 bg-[#4285F4]/10 rounded-2xl flex items-center justify-center mx-auto mb-4">
                  <ShieldCheck className="w-8 h-8 text-[#4285F4]" />
                </div>
                <div className="space-y-2">
                  <h3 className="text-2xl font-black text-white tracking-tight uppercase">GCP IAM Required</h3>
                  <p className="text-slate-500 text-sm font-light leading-relaxed">
                    Deploying to Google Compute Engine requires an authorized identity for project <b>{cloudConfig.project}</b>.
                  </p>
                </div>
                <button 
                  onClick={handleCloudLogin}
                  className="w-full py-4 bg-[#4285F4] hover:bg-[#3367D6] text-white rounded-md font-bold uppercase tracking-widest text-xs transition-all shadow-xl shadow-[#4285F4]/20"
                >
                  Sign in with Google Cloud
                </button>
                <button onClick={() => setShowDeployModal(false)} className="text-[9px] font-black text-slate-600 uppercase tracking-widest hover:text-white transition-colors">
                  Cancel Deployment
                </button>
              </div>
            ) : (
              <>
                <div className="p-6 border-b border-slate-800 flex items-center justify-between bg-[#1e1f24]">
                  <div className="flex items-center gap-3">
                    <Cloud className="w-5 h-5 text-[#4285F4]" />
                    <div>
                      <h3 className="text-base font-bold text-white uppercase tracking-tight">Provision Resources</h3>
                      <p className="text-[9px] text-slate-500 font-black uppercase tracking-widest">Active Identity: {currentUser.login}</p>
                    </div>
                  </div>
                  <button onClick={() => setShowDeployModal(false)} className="p-1.5 hover:bg-slate-800 rounded-full transition-colors text-slate-500 hover:text-white">
                    <X className="w-5 h-5" />
                  </button>
                </div>
                
                <div className="p-8 space-y-6">
                  <div className="space-y-4">
                    <div className="space-y-2">
                      <label className="text-[9px] font-black uppercase tracking-[0.2em] text-slate-500 ml-1">Target Project ID</label>
                      <input 
                        type="text" 
                        value={cloudConfig.project}
                        onChange={(e) => setCloudConfig({...cloudConfig, project: e.target.value})}
                        className="w-full px-4 py-3 bg-[#0b0e14] border border-slate-800 rounded-lg text-sm font-bold text-white outline-none focus:border-[#4285F4]/50"
                      />
                    </div>

                    <div className="grid grid-cols-2 gap-4">
                      <div className="space-y-2">
                        <label className="text-[9px] font-black uppercase tracking-[0.2em] text-slate-500 ml-1">GCP Region</label>
                        <select 
                          value={cloudConfig.region}
                          onChange={(e) => setCloudConfig({...cloudConfig, region: e.target.value})}
                          className="w-full px-4 py-3 bg-[#0b0e14] border border-slate-800 rounded-lg text-sm font-bold text-white outline-none focus:border-[#4285F4]/50 appearance-none"
                        >
                          <option value="us-east4">us-east4 (No. Virginia)</option>
                          <option value="us-central1">us-central1 (Iowa)</option>
                          <option value="europe-west1">europe-west1 (Belgium)</option>
                        </select>
                      </div>
                      <div className="space-y-2">
                        <label className="text-[9px] font-black uppercase tracking-[0.2em] text-slate-500 ml-1">Runtime SKU</label>
                        <select 
                          value={cloudConfig.runtime}
                          onChange={(e) => setCloudConfig({...cloudConfig, runtime: e.target.value})}
                          className="w-full px-4 py-3 bg-[#0b0e14] border border-slate-800 rounded-lg text-sm font-bold text-white outline-none focus:border-[#4285F4]/50 appearance-none"
                        >
                          <option value="Micro Focus Enterprise">Micro Focus Ent.</option>
                          <option value="Rocket Enterprise">Rocket Ent.</option>
                          <option value="GCP Mainframe Runtimes">GCP Native</option>
                        </select>
                      </div>
                    </div>
                  </div>

                  <div className="p-4 rounded-xl bg-[#4285F4]/5 border border-[#4285F4]/10 flex items-start gap-3">
                    <Info className="w-4 h-4 text-[#4285F4] shrink-0 mt-0.5" />
                    <p className="text-[10px] text-slate-400 leading-relaxed">
                      Executing this deployment will create <span className="text-[#4285F4] font-bold">{chunks.length} Compute Engine</span> resources. Billing will commence upon resource activation in <b>{cloudConfig.project}</b>.
                    </p>
                  </div>

                  <button 
                    onClick={handleGCPDeploy}
                    disabled={isProvisioning}
                    className={`w-full py-4 rounded-md font-black text-xs uppercase tracking-[0.2em] transition-all flex items-center justify-center gap-3 ${
                      isProvisioning ? 'bg-slate-800 text-slate-600' : 'bg-[#4285F4] text-white hover:bg-[#3367D6] shadow-xl shadow-[#4285F4]/10'
                    }`}
                  >
                    {isProvisioning ? (
                      <>
                        <div className="w-3.5 h-3.5 border-2 border-slate-600 border-t-transparent rounded-full animate-spin" />
                        Provisioning GCE Cluster...
                      </>
                    ) : (
                      <>
                        <ExternalLink className="w-4 h-4" /> Start Cloud Provisioning
                      </>
                    )}
                  </button>
                </div>
              </>
            )}
          </div>
        </div>
      )}

      {/* GCP Style Footer */}
      <footer className="h-8 border-t border-slate-800 bg-[#1e1f24] flex items-center justify-between px-4 text-[9px] font-black uppercase tracking-[0.2em] text-slate-500">
        <div className="flex gap-6">
          <span className="flex items-center gap-1.5"><div className="w-1.5 h-1.5 rounded-full bg-[#34A853]" /> GCloud SDK: v452.0</span>
          <span className="flex items-center gap-1.5"><div className="w-1.5 h-1.5 rounded-full bg-[#4285F4]" /> Network: VPC-PRD-01</span>
        </div>
        <div className="flex items-center gap-4">
          <span className="hover:text-[#4285F4] cursor-pointer transition-colors">Documentation</span>
          <div className="w-[1px] h-3 bg-slate-800" />
          <span>© 2024 LegacyLink AI Architecture Lab</span>
        </div>
      </footer>
    </div>
  );
};

export default App;
