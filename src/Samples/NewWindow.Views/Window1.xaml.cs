using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Threading;

namespace Elmish.WPF.Samples.NewWindow {
  public partial class Window1 : Window {
    public Window1() {
      InitializeComponent();
    }

    // Based on http://reedcopsey.com/2011/11/28/launching-a-wpf-window-in-a-separate-thread-part-1/
    public static Task<Window> Create() {
      var tcs = new TaskCompletionSource<Window>();
      var thread = new Thread(new ThreadStart(() =>
      {
        // Create our context, and install it:
        SynchronizationContext.SetSynchronizationContext(
            new DispatcherSynchronizationContext(
                Dispatcher.CurrentDispatcher));

        var window = new Window1();
        // When the window closes, shut down the dispatcher
        window.Closed += (s, e) =>
           Dispatcher.CurrentDispatcher.BeginInvokeShutdown(DispatcherPriority.Background);

        tcs.SetResult(window);

        // Start the Dispatcher Processing
        Dispatcher.Run();
      }));
      // Set the apartment state
      thread.SetApartmentState(ApartmentState.STA);
      // Make the thread a background thread
      thread.IsBackground = true;
      // Start the thread
      thread.Start();
      return tcs.Task;
    }

  }
}
