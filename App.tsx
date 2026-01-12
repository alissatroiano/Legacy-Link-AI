
import React, { useState, useEffect, useCallback, useRef } from 'react';
import { 
  Terminal, Settings, Cpu, Database, Layers, CheckCircle2, AlertCircle, 
  Clock, Cloud, ArrowUpRight, Upload, PlayCircle, FileCode2, Download, 
  Github, GitBranch, X, ShieldCheck, ExternalLink, Save, Info, User, 
  LogOut, Server, Globe, HardDrive, Activity, Code2, Zap, Layout
} from 'lucide-react';
import { 
  BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, Cell
} from 'recharts';
import { geminiService } from './services/geminiService';
import { CodeChunk, ConversionStep, GitHubConfig, GitHubUser, ModernizationStrategy } from './types';

const MOCK_COBOL = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER-PROC-01.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "CORE BANKING LEDGER START".
           STOP RUN.
`;

const App: React.FC = () => {
  const [step, setStep] = useState<ConversionStep>(ConversionStep.IDLE);
  const [strategy, setStrategy] = useState<ModernizationStrategy>('hybrid');
  const [chunks, setChunks] = useState<CodeChunk[]>([]);
  const [selectedChunkId, setSelectedChunkId] = useState<string | null>(null);
  const [logs, setLogs] = useState<string[]>([]);
  const [activeTab, setActiveTab] = useState<'refactor' | 'rehost'>('refactor');
  
  const [ghUser, setGhUser] = useState<GitHubUser | null>(null);
  const [gcpUser, setGcpUser] = useState<GitHubUser | null>(null);
  const [showSyncModal, setShowSyncModal] = useState(false);
  const [isSyncing, setIsSyncing] = useState(false);

  const logEndRef = useRef<HTMLDivElement>(null);
  const addLog = (msg: string) => setLogs(prev => [...prev, `[${new Date().toLocaleTimeString()}] ${msg}`]);

  useEffect(() => { logEndRef.current?.scrollIntoView({ behavior: 'smooth' }); }, [logs]);

  const handleAuth = (type: 'github' | 'gcp') => {
    addLog(`Authorizing ${type.toUpperCase()} credentials...`);
    setTimeout(() => {
      const mock: GitHubUser = { id: '1', login: 'arch_user', name: 'Lead Architect', avatar_url: `https://api.dicebear.com/7.x/avataaars/svg?seed=${type}`, role: 'Lead' };
      if (type === 'github') setGhUser(mock);
      else setGcpUser(mock);
      addLog(`${type.toUpperCase()} Handshake Successful.`);
    }, 1000);
  };

  const handleFileUpload = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;
    setStep(ConversionStep.ANALYZING_SYSTEM);
    addLog(`LegacyLink AI: Deconstructing ${file.name}...`);
    try {
      const reader = new FileReader();
      reader.onload = async (evt) => {
        const analysis = await geminiService.analyzeSystem(evt.target?.result as string, file.name);
        setChunks(analysis.map((item: any, idx: number) => ({ ...item, id: `mod-${idx}`, cobolCode: item.code, status: 'pending', progress: 0 })));
        setStep(ConversionStep.CONVERTING_INCREMENTS);
        addLog(`System parsed into ${analysis.length} increments.`);
      };
      reader.readAsText(file);
    } catch (err) { setStep(ConversionStep.IDLE); }
  };

  const processNext = useCallback(async () => {
    const next = chunks.find(c => c.status === 'pending');
    if (!next) {
      if (chunks.length > 0 && chunks.every(c => c.status === 'completed')) setStep(ConversionStep.FINALIZING);
      return;
    }
    setChunks(prev => prev.map(c => c.id === next.id ? { ...c, status: 'processing', progress: 45 } : c));
    addLog(`Mapping Increment: ${next.name}`);
    try {
      const res = await geminiService.processIncrement(next.cobolCode, next.recommendedInstance || 'n2-standard-4');
      setChunks(prev => prev.map(c => c.id === next.id ? { ...c, ...res, status: 'completed', progress: 100 } : c));
      addLog(`Validated: ${next.name}`);
    } catch (err) {
      setChunks(prev => prev.map(c => c.id === next.id ? { ...c, status: 'error' } : c));
    }
  }, [chunks]);

  useEffect(() => {
    if (step === ConversionStep.CONVERTING_INCREMENTS) {
      const t = setTimeout(processNext, 1200);
      return () => clearTimeout(t);
    }
  }, [step, processNext]);

  const selectedChunk = chunks.find(c => c.id === selectedChunkId);

  return (
    <div className="min-h-screen flex flex-col bg-[#0b0e14] text-slate-200 font-sans">
      {/* Dynamic Hybrid Header */}
      <header className="h-14 border-b border-slate-800 bg-[#111318] flex items-center justify-between px-6 sticky top-0 z-50">
        <div className="flex items-center gap-4">
          <div className="w-8 h-8 bg-indigo-600 rounded-lg flex items-center justify-center">
            <Cpu className="w-5 h-5 text-white" />
          </div>
          <h1 className="text-sm font-black tracking-tight uppercase">LegacyLink <span className="text-indigo-400">AI</span></h1>
        </div>

        <div className="flex items-center gap-6">
          <div className="flex items-center gap-2 px-3 py-1 bg-slate-900 border border-slate-800 rounded-full">
            <div className={`w-2 h-2 rounded-full ${ghUser ? 'bg-indigo-500' : 'bg-slate-700'}`} />
            <Github className="w-3 h-3 text-slate-400" />
            <div className="w-px h-3 bg-slate-800" />
            <div className={`w-2 h-2 rounded-full ${gcpUser ? 'bg-blue-500' : 'bg-slate-700'}`} />
            <Cloud className="w-3 h-3 text-slate-400" />
          </div>
          {!ghUser || !gcpUser ? (
            <button onClick={() => handleAuth(ghUser ? 'gcp' : 'github')} className="text-[10px] font-black uppercase tracking-widest text-indigo-400 hover:text-white transition-colors">
              Link Systems
            </button>
          ) : (
            <div className="flex items-center gap-2">
               <span className="text-[10px] font-bold text-slate-500">SESSION ACTIVE</span>
               <img src={ghUser.avatar_url} className="w-7 h-7 rounded-full border border-slate-700" />
            </div>
          )}
        </div>
      </header>

      <main className="flex-1 flex overflow-hidden">
        {/* Module Sidebar */}
        <aside className="w-72 border-r border-slate-800 flex flex-col bg-[#111318]">
          <div className="p-4 border-b border-slate-800 bg-slate-900/50 flex justify-between items-center">
            <h2 className="text-[10px] font-black uppercase tracking-widest text-slate-500">Migration Queue</h2>
            <Activity className="w-3 h-3 text-indigo-500" />
          </div>
          <div className="flex-1 overflow-y-auto p-2 space-y-1">
            {chunks.map(chunk => (
              <button key={chunk.id} onClick={() => setSelectedChunkId(chunk.id)} className={`w-full p-3 rounded-lg text-left transition-all border ${selectedChunkId === chunk.id ? 'bg-indigo-600/10 border-indigo-500/30' : 'border-transparent hover:bg-slate-800/40'}`}>
                <div className="flex items-center gap-3 mb-2">
                  <div className={`w-2 h-2 rounded-full ${chunk.status === 'completed' ? 'bg-emerald-500' : 'bg-slate-700 animate-pulse'}`} />
                  <span className="text-[11px] font-bold truncate flex-1">{chunk.name}</span>
                </div>
                <div className="h-1 w-full bg-slate-800 rounded-full overflow-hidden">
                  <div className="h-full bg-indigo-500 transition-all duration-500" style={{ width: `${chunk.progress}%` }} />
                </div>
              </button>
            ))}
            {chunks.length === 0 && <div className="p-8 text-center opacity-20"><FileCode2 className="mx-auto mb-4" /> <span className="text-xs uppercase font-black">Ready for Payload</span></div>}
          </div>
          <div className="p-4 border-t border-slate-800">
             <label className="flex flex-col items-center justify-center w-full h-16 border border-dashed border-slate-700 rounded-xl cursor-pointer hover:border-indigo-500 transition-all group">
                <Upload className="w-4 h-4 text-slate-500 group-hover:text-indigo-400" />
                <span className="text-[9px] font-black uppercase text-slate-500 mt-1">Ingest COBOL</span>
                <input type="file" className="hidden" onChange={handleFileUpload} />
             </label>
          </div>
        </aside>

        {/* Workspace */}
        <section className="flex-1 flex flex-col bg-[#0b0e14]">
          {step === ConversionStep.IDLE ? (
             <div className="flex-1 flex flex-col items-center justify-center p-12 text-center max-w-3xl mx-auto">
                <div className="w-20 h-20 bg-indigo-600/10 rounded-3xl flex items-center justify-center mb-8">
                  <Zap className="w-10 h-10 text-indigo-400" />
                </div>
                <h2 className="text-4xl font-black uppercase tracking-tight mb-4">Dual-Path Modernization</h2>
                <p className="text-slate-400 mb-12 leading-relaxed">Translate legacy COBOL into cloud-native Python 3.12 while simultaneously provisioning Google Compute Engine rehosting infrastructure.</p>
                
                <div className="grid grid-cols-2 gap-4 w-full">
                   <div className="p-6 bg-slate-900/50 border border-slate-800 rounded-2xl text-left">
                      <Github className="w-6 h-6 text-white mb-4" />
                      <h3 className="font-bold text-sm mb-2">Code Refactoring</h3>
                      <p className="text-[11px] text-slate-500">Incremental logic rewrite to modern Pythonic microservices with 100% parity testing.</p>
                   </div>
                   <div className="p-6 bg-slate-900/50 border border-slate-800 rounded-2xl text-left">
                      <Cloud className="w-6 h-6 text-blue-400 mb-4" />
                      <h3 className="font-bold text-sm mb-2">Cloud Rehosting</h3>
                      <p className="text-[11px] text-slate-500">Automated Terraform provisioning for GCE instances using partner runtimes.</p>
                   </div>
                </div>
                <button onClick={() => { const e = { target: { files: [new File([MOCK_COBOL], "LEDGER.CBL")] } } as any; handleFileUpload(e); }} className="mt-12 px-8 py-3 bg-white text-black rounded-xl font-black text-xs uppercase tracking-widest hover:bg-indigo-50 transition-all">Start Hybrid Migration</button>
             </div>
          ) : (
            <div className="flex-1 flex flex-col overflow-hidden">
               <div className="h-12 border-b border-slate-800 flex items-center px-4 gap-4 bg-slate-900/20">
                  <button onClick={() => setActiveTab('refactor')} className={`px-4 h-full text-[10px] font-black uppercase tracking-widest border-b-2 transition-all ${activeTab === 'refactor' ? 'border-indigo-500 text-indigo-400' : 'border-transparent text-slate-500'}`}>Refactor: Python</button>
                  <button onClick={() => setActiveTab('rehost')} className={`px-4 h-full text-[10px] font-black uppercase tracking-widest border-b-2 transition-all ${activeTab === 'rehost' ? 'border-blue-500 text-blue-400' : 'border-transparent text-slate-500'}`}>Rehost: GCE</button>
               </div>

               <div className="flex-1 flex overflow-hidden">
                  <div className="flex-1 border-r border-slate-800 flex flex-col">
                     <div className="p-3 bg-slate-950 border-b border-slate-800 text-[9px] font-black text-slate-500 uppercase tracking-widest">Legacy Source (COBOL)</div>
                     <div className="flex-1 p-6 font-mono text-xs text-slate-500 overflow-auto">{selectedChunk?.cobolCode || "Select module..."}</div>
                  </div>
                  <div className="flex-1 flex flex-col">
                     <div className="p-3 bg-slate-950 border-b border-slate-800 flex justify-between items-center">
                        <span className="text-[9px] font-black text-slate-500 uppercase tracking-widest">{activeTab === 'refactor' ? 'Target: Python 3.12' : 'Infrastructure: Terraform'}</span>
                        {selectedChunk?.status === 'completed' && <div className="text-[8px] font-black text-emerald-500 bg-emerald-500/10 px-2 py-0.5 rounded">READY</div>}
                     </div>
                     <div className="flex-1 p-6 font-mono text-xs overflow-auto bg-slate-900/10">
                        {activeTab === 'refactor' ? (
                           <pre className="text-indigo-300">{selectedChunk?.pythonCode || "Awaiting Refactor Wave..."}</pre>
                        ) : (
                           <pre className="text-blue-300">{selectedChunk?.terraform || "Awaiting Cloud Mapping..."}</pre>
                        )}
                     </div>
                  </div>
               </div>
            </div>
          )}
        </section>

        {/* Global Control Sidebar */}
        <aside className="w-80 border-l border-slate-800 flex flex-col bg-[#111318]">
          <div className="p-4 border-b border-slate-800 bg-slate-900/50 flex justify-between items-center">
            <h2 className="text-[10px] font-black uppercase tracking-widest text-slate-500">Operation Monitor</h2>
            <Terminal className="w-3 h-3 text-emerald-500" />
          </div>
          <div className="flex-1 overflow-y-auto p-4 font-mono text-[9px] space-y-1 bg-[#0b0e14]">
             {logs.map((log, i) => <div key={i} className="text-slate-500"><span className="text-indigo-400">$</span> {log}</div>)}
             <div ref={logEndRef} />
          </div>
          <div className="p-6 border-t border-slate-800 bg-[#1e1f24]/50 space-y-4">
             <button onClick={() => handleAuth('github')} className="w-full flex items-center justify-between p-3 bg-slate-900 rounded-xl border border-slate-800 hover:border-indigo-500/50 transition-all">
                <div className="flex items-center gap-3">
                   <Github className={`w-4 h-4 ${ghUser ? 'text-indigo-400' : 'text-slate-600'}`} />
                   <span className="text-[10px] font-black uppercase">GitHub Sync</span>
                </div>
                {ghUser && <CheckCircle2 className="w-3 h-3 text-emerald-500" />}
             </button>
             <button onClick={() => handleAuth('gcp')} className="w-full flex items-center justify-between p-3 bg-slate-900 rounded-xl border border-slate-800 hover:border-blue-500/50 transition-all">
                <div className="flex items-center gap-3">
                   <Cloud className={`w-4 h-4 ${gcpUser ? 'text-blue-400' : 'text-slate-600'}`} />
                   <span className="text-[10px] font-black uppercase">GCP Provision</span>
                </div>
                {gcpUser && <CheckCircle2 className="w-3 h-3 text-emerald-500" />}
             </button>
             <div className="pt-2">
                <button 
                  disabled={chunks.length === 0}
                  className="w-full py-4 bg-white text-black rounded-xl font-black text-[10px] uppercase tracking-widest shadow-xl shadow-white/5 disabled:opacity-20"
                >
                  Execute Global Sync
                </button>
             </div>
          </div>
        </aside>
      </main>

      <footer className="h-8 border-t border-slate-800 bg-[#1e1f24] flex items-center justify-between px-6 text-[9px] font-black uppercase text-slate-600">
         <div className="flex gap-6 items-center">
            <span className="flex items-center gap-2"><div className="w-1.5 h-1.5 rounded-full bg-emerald-500" /> SOC2 COMPLIANT</span>
            <span className="flex items-center gap-2"><div className="w-1.5 h-1.5 rounded-full bg-indigo-500" /> GEMINI-PRO ACTIVATED</span>
         </div>
         <span>© 2024 LegacyLink AI Architecture Lab</span>
      </footer>
    </div>
  );
};

export default App;
