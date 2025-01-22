package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.List;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.AbstractCvsTask.Module;
import org.apache.tools.ant.types.Commandline;
import org.junit.Test;

public class AbstractCvsTaskDiffblueTest {
  /**
   * Test Module getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Module}
   *   <li>{@link Module#setName(String)}
   *   <li>{@link Module#getName()}
   * </ul>
   */
  @Test
  public void testModuleGettersAndSetters() {
    // Arrange and Act
    Module actualResultModule = new Module();
    actualResultModule.setName("foo");

    // Assert
    assertEquals("foo", actualResultModule.getName());
  }

  /**
   * Test {@link AbstractCvsTask#setExecuteStreamHandler(ExecuteStreamHandler)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#setExecuteStreamHandler(ExecuteStreamHandler)}
   */
  @Test
  public void testSetExecuteStreamHandler() {
    // Arrange
    Cvs cvs = new Cvs();
    PumpStreamHandler handler = new PumpStreamHandler();

    // Act
    cvs.setExecuteStreamHandler(handler);

    // Assert
    assertSame(handler, cvs.getExecuteStreamHandler());
  }

  /**
   * Test {@link AbstractCvsTask#setOutputStream(OutputStream)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#setOutputStream(OutputStream)}
   */
  @Test
  public void testSetOutputStream() {
    // Arrange
    Cvs cvs = new Cvs();
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream(1);

    // Act
    cvs.setOutputStream(outputStream);

    // Assert
    ExecuteStreamHandler executeStreamHandler = cvs.getExecuteStreamHandler();
    assertTrue(executeStreamHandler instanceof PumpStreamHandler);
    assertSame(outputStream, cvs.getOutputStream());
    assertSame(outputStream, ((PumpStreamHandler) executeStreamHandler).getOut());
  }

  /**
   * Test {@link AbstractCvsTask#setErrorStream(OutputStream)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#setErrorStream(OutputStream)}
   */
  @Test
  public void testSetErrorStream() {
    // Arrange
    Cvs cvs = new Cvs();
    ByteArrayOutputStream errorStream = new ByteArrayOutputStream(1);

    // Act
    cvs.setErrorStream(errorStream);

    // Assert
    ExecuteStreamHandler executeStreamHandler = cvs.getExecuteStreamHandler();
    assertTrue(executeStreamHandler instanceof PumpStreamHandler);
    assertSame(errorStream, cvs.getErrorStream());
    assertSame(errorStream, ((PumpStreamHandler) executeStreamHandler).getErr());
  }

  /**
   * Test {@link AbstractCvsTask#runCommand(Commandline)}.
   * <ul>
   *   <li>Given {@link Cvs} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Cvs} (default constructor) Dest Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#runCommand(Commandline)}
   */
  @Test
  public void testRunCommand_givenCvsProjectIsProject_thenCvsDestNameIsApacheAnt11015() throws BuildException {
    // Arrange
    Cvs cvs = new Cvs();
    cvs.setProject(new Project());

    // Act
    cvs.runCommand(new Commandline("To Process"));

    // Assert
    File dest = cvs.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvs.getProject().getBaseDir());
  }

  /**
   * Test {@link AbstractCvsTask#runCommand(Commandline)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#runCommand(Commandline)}
   */
  @Test
  public void testRunCommand_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Cvs cvs = new Cvs();
    cvs.setProject(project);

    // Act
    cvs.runCommand(new Commandline("To Process"));

    // Assert
    File dest = cvs.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvs.getProject().getBaseDir());
  }

  /**
   * Test {@link AbstractCvsTask#runCommand(Commandline)}.
   * <ul>
   *   <li>Then {@link Cvs} (default constructor) Dest Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#runCommand(Commandline)}
   */
  @Test
  public void testRunCommand_thenCvsDestNameIsTestTxt() throws BuildException {
    // Arrange
    Cvs cvs = new Cvs();
    cvs.setDest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    cvs.runCommand(new Commandline("To Process"));

    // Assert that nothing has changed
    File dest = cvs.getDest();
    assertEquals("test.txt", dest.getName());
    assertTrue(dest.isAbsolute());
  }

  /**
   * Test {@link AbstractCvsTask#runCommand(Commandline)}.
   * <ul>
   *   <li>When {@link Commandline#Commandline(String)} with toProcess is {@code cygwin.user.home}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#runCommand(Commandline)}
   */
  @Test
  public void testRunCommand_whenCommandlineWithToProcessIsCygwinUserHome() throws BuildException {
    // Arrange
    Cvs cvs = new Cvs();
    cvs.setProject(new Project());

    // Act
    cvs.runCommand(new Commandline("cygwin.user.home"));

    // Assert
    File dest = cvs.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvs.getProject().getBaseDir());
  }

  /**
   * Test {@link AbstractCvsTask#runCommand(Commandline)}.
   * <ul>
   *   <li>When {@link Commandline#Commandline(String)} with toProcess is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#runCommand(Commandline)}
   */
  @Test
  public void testRunCommand_whenCommandlineWithToProcessIsNull() throws BuildException {
    // Arrange
    Cvs cvs = new Cvs();
    cvs.setProject(new Project());

    // Act
    cvs.runCommand(new Commandline(null));

    // Assert
    File dest = cvs.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvs.getProject().getBaseDir());
  }

  /**
   * Test {@link AbstractCvsTask#execute()}.
   * <ul>
   *   <li>Given {@link Cvs} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Cvs} (default constructor) Dest Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#execute()}
   */
  @Test
  public void testExecute_givenCvsProjectIsProject_thenCvsDestNameIsApacheAnt11015() throws BuildException {
    // Arrange
    Cvs cvs = new Cvs();
    cvs.setProject(new Project());
    cvs.addConfiguredCommandline(new Commandline("checkout"));

    // Act
    cvs.execute();

    // Assert
    File dest = cvs.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvs.getProject().getBaseDir());
  }

  /**
   * Test {@link AbstractCvsTask#execute()}.
   * <ul>
   *   <li>Given {@link Cvs} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Cvs} (default constructor) Dest Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#execute()}
   */
  @Test
  public void testExecute_givenCvsProjectIsProject_thenCvsDestNameIsApacheAnt110152() throws BuildException {
    // Arrange
    Module m = new Module();
    m.setName("checkout");

    Cvs cvs = new Cvs();
    cvs.setProject(new Project());
    cvs.addModule(m);

    // Act
    cvs.execute();

    // Assert
    File dest = cvs.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvs.getProject().getBaseDir());
  }

  /**
   * Test {@link AbstractCvsTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Cvs cvs = new Cvs();
    cvs.setProject(project);
    cvs.addConfiguredCommandline(new Commandline("checkout"));

    // Act
    cvs.execute();

    // Assert
    File dest = cvs.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvs.getProject().getBaseDir());
  }

  /**
   * Test {@link AbstractCvsTask#execute()}.
   * <ul>
   *   <li>Then {@link Cvs} (default constructor) Dest Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#execute()}
   */
  @Test
  public void testExecute_thenCvsDestNameIsTestTxt() throws BuildException {
    // Arrange
    Cvs cvs = new Cvs();
    cvs.setDest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cvs.addConfiguredCommandline(new Commandline("checkout"));

    // Act
    cvs.execute();

    // Assert that nothing has changed
    File dest = cvs.getDest();
    assertEquals("test.txt", dest.getName());
    assertTrue(dest.isAbsolute());
  }

  /**
   * Test {@link AbstractCvsTask#setCvsRoot(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link Cvs} (default constructor) CvsRoot is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setCvsRoot(String)}
   */
  @Test
  public void testSetCvsRoot_whenEmptyString_thenCvsCvsRootIsNull() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setCvsRoot("");

    // Assert that nothing has changed
    assertNull(cvs.getCvsRoot());
  }

  /**
   * Test {@link AbstractCvsTask#setCvsRoot(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Cvs} (default constructor) CvsRoot is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setCvsRoot(String)}
   */
  @Test
  public void testSetCvsRoot_whenNull_thenCvsCvsRootIsNull() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setCvsRoot(null);

    // Assert that nothing has changed
    assertNull(cvs.getCvsRoot());
  }

  /**
   * Test {@link AbstractCvsTask#setCvsRoot(String)}.
   * <ul>
   *   <li>When {@code Root}.</li>
   *   <li>Then {@link Cvs} (default constructor) CvsRoot is {@code Root}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setCvsRoot(String)}
   */
  @Test
  public void testSetCvsRoot_whenRoot_thenCvsCvsRootIsRoot() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setCvsRoot("Root");

    // Assert
    assertEquals("Root", cvs.getCvsRoot());
  }

  /**
   * Test {@link AbstractCvsTask#getCvsRoot()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getCvsRoot()}
   */
  @Test
  public void testGetCvsRoot() {
    // Arrange, Act and Assert
    assertNull((new Cvs()).getCvsRoot());
  }

  /**
   * Test {@link AbstractCvsTask#setCvsRsh(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link Cvs} (default constructor) CvsRsh is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setCvsRsh(String)}
   */
  @Test
  public void testSetCvsRsh_whenEmptyString_thenCvsCvsRshIsNull() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setCvsRsh("");

    // Assert that nothing has changed
    assertNull(cvs.getCvsRsh());
  }

  /**
   * Test {@link AbstractCvsTask#setCvsRsh(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Cvs} (default constructor) CvsRsh is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setCvsRsh(String)}
   */
  @Test
  public void testSetCvsRsh_whenNull_thenCvsCvsRshIsNull() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setCvsRsh(null);

    // Assert that nothing has changed
    assertNull(cvs.getCvsRsh());
  }

  /**
   * Test {@link AbstractCvsTask#setCvsRsh(String)}.
   * <ul>
   *   <li>When {@code Rsh}.</li>
   *   <li>Then {@link Cvs} (default constructor) CvsRsh is {@code Rsh}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setCvsRsh(String)}
   */
  @Test
  public void testSetCvsRsh_whenRsh_thenCvsCvsRshIsRsh() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setCvsRsh("Rsh");

    // Assert
    assertEquals("Rsh", cvs.getCvsRsh());
  }

  /**
   * Test {@link AbstractCvsTask#getCvsRsh()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getCvsRsh()}
   */
  @Test
  public void testGetCvsRsh() {
    // Arrange, Act and Assert
    assertNull((new Cvs()).getCvsRsh());
  }

  /**
   * Test {@link AbstractCvsTask#setPort(int)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#setPort(int)}
   */
  @Test
  public void testSetPort() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setPort(8080);

    // Assert
    assertEquals(8080, cvs.getPort());
  }

  /**
   * Test {@link AbstractCvsTask#getPort()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getPort()}
   */
  @Test
  public void testGetPort() {
    // Arrange, Act and Assert
    assertEquals(0, (new Cvs()).getPort());
  }

  /**
   * Test {@link AbstractCvsTask#setPassfile(File)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#setPassfile(File)}
   */
  @Test
  public void testSetPassfile() {
    // Arrange
    Cvs cvs = new Cvs();
    File passFile = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    cvs.setPassfile(passFile);

    // Assert
    assertSame(passFile, cvs.getPassFile());
  }

  /**
   * Test {@link AbstractCvsTask#getPassFile()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getPassFile()}
   */
  @Test
  public void testGetPassFile() {
    // Arrange, Act and Assert
    assertNull((new Cvs()).getPassFile());
  }

  /**
   * Test {@link AbstractCvsTask#setDest(File)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#setDest(File)}
   */
  @Test
  public void testSetDest() {
    // Arrange
    Cvs cvs = new Cvs();
    File dest = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    cvs.setDest(dest);

    // Assert
    assertSame(dest, cvs.getDest());
  }

  /**
   * Test {@link AbstractCvsTask#getDest()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getDest()}
   */
  @Test
  public void testGetDest() {
    // Arrange, Act and Assert
    assertNull((new Cvs()).getDest());
  }

  /**
   * Test {@link AbstractCvsTask#setPackage(String)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#setPackage(String)}
   */
  @Test
  public void testSetPackage() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setPackage("foo");

    // Assert
    assertEquals("foo", cvs.getPackage());
  }

  /**
   * Test {@link AbstractCvsTask#getPackage()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getPackage()}
   */
  @Test
  public void testGetPackage() {
    // Arrange, Act and Assert
    assertNull((new Cvs()).getPackage());
  }

  /**
   * Test {@link AbstractCvsTask#getTag()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getTag()}
   */
  @Test
  public void testGetTag() {
    // Arrange, Act and Assert
    assertNull((new Cvs()).getTag());
  }

  /**
   * Test {@link AbstractCvsTask#setTag(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link Cvs} (default constructor) Tag is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setTag(String)}
   */
  @Test
  public void testSetTag_whenEmptyString_thenCvsTagIsNull() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setTag("");

    // Assert that nothing has changed
    assertNull(cvs.getTag());
  }

  /**
   * Test {@link AbstractCvsTask#setTag(String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then {@link Cvs} (default constructor) Tag is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setTag(String)}
   */
  @Test
  public void testSetTag_whenFoo_thenCvsTagIsFoo() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setTag("foo");

    // Assert
    assertEquals("foo", cvs.getTag());
  }

  /**
   * Test {@link AbstractCvsTask#setTag(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Cvs} (default constructor) Tag is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#setTag(String)}
   */
  @Test
  public void testSetTag_whenNull_thenCvsTagIsNull() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setTag(null);

    // Assert that nothing has changed
    assertNull(cvs.getTag());
  }

  /**
   * Test {@link AbstractCvsTask#setCommand(String)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#setCommand(String)}
   */
  @Test
  public void testSetCommand() {
    // Arrange
    Cvs cvs = new Cvs();

    // Act
    cvs.setCommand("foo");

    // Assert
    assertEquals("foo", cvs.getCommand());
  }

  /**
   * Test {@link AbstractCvsTask#getCommand()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getCommand()}
   */
  @Test
  public void testGetCommand() {
    // Arrange, Act and Assert
    assertNull((new Cvs()).getCommand());
  }

  /**
   * Test {@link AbstractCvsTask#configureCommandline(Commandline)}.
   * <ul>
   *   <li>Given {@link Cvs} (default constructor) Package is {@code cvs}.</li>
   *   <li>Then second element is {@code cvs}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#configureCommandline(Commandline)}
   */
  @Test
  public void testConfigureCommandline_givenCvsPackageIsCvs_thenSecondElementIsCvs() {
    // Arrange
    Module m = new Module();
    m.setName("cvs");

    Cvs cvs = new Cvs();
    cvs.setPackage("cvs");
    cvs.addModule(m);
    Commandline c = new Commandline("To Process");

    // Act
    cvs.configureCommandline(c);

    // Assert
    String[] arguments = c.getArguments();
    assertEquals("cvs", arguments[1]);
    assertEquals("cvs", arguments[2]);
    String[] commandline = c.getCommandline();
    assertEquals("cvs", commandline[2]);
    assertEquals("cvs", commandline[3]);
    assertEquals(3, arguments.length);
    assertEquals(4, c.size());
    assertEquals(4, commandline.length);
  }

  /**
   * Test {@link AbstractCvsTask#addConfiguredCommandline(Commandline, boolean)} with {@code c}, {@code insertAtStart}.
   * <ul>
   *   <li>Then second element is {@code cvs}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#addConfiguredCommandline(Commandline, boolean)}
   */
  @Test
  public void testAddConfiguredCommandlineWithCInsertAtStart_thenSecondElementIsCvs() {
    // Arrange
    Module m = new Module();
    m.setName("cvs");

    Cvs cvs = new Cvs();
    cvs.setPackage("cvs");
    cvs.addModule(m);
    Commandline c = new Commandline("To Process");

    // Act
    cvs.addConfiguredCommandline(c, true);

    // Assert
    String[] arguments = c.getArguments();
    assertEquals("cvs", arguments[1]);
    assertEquals("cvs", arguments[2]);
    String[] commandline = c.getCommandline();
    assertEquals("cvs", commandline[2]);
    assertEquals("cvs", commandline[3]);
    assertEquals(3, arguments.length);
    assertEquals(4, c.size());
    assertEquals(4, commandline.length);
  }

  /**
   * Test {@link AbstractCvsTask#addConfiguredCommandline(Commandline)} with {@code c}.
   * <ul>
   *   <li>Given {@link Cvs} (default constructor) Package is {@code cvs}.</li>
   *   <li>Then second element is {@code cvs}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractCvsTask#addConfiguredCommandline(Commandline)}
   */
  @Test
  public void testAddConfiguredCommandlineWithC_givenCvsPackageIsCvs_thenSecondElementIsCvs() {
    // Arrange
    Module m = new Module();
    m.setName("cvs");

    Cvs cvs = new Cvs();
    cvs.setPackage("cvs");
    cvs.addModule(m);
    Commandline c = new Commandline("To Process");

    // Act
    cvs.addConfiguredCommandline(c);

    // Assert
    String[] arguments = c.getArguments();
    assertEquals("cvs", arguments[1]);
    assertEquals("cvs", arguments[2]);
    String[] commandline = c.getCommandline();
    assertEquals("cvs", commandline[2]);
    assertEquals("cvs", commandline[3]);
    assertEquals(3, arguments.length);
    assertEquals(4, c.size());
    assertEquals(4, commandline.length);
  }

  /**
   * Test {@link AbstractCvsTask#addModule(Module)}.
   * <p>
   * Method under test: {@link AbstractCvsTask#addModule(Module)}
   */
  @Test
  public void testAddModule() {
    // Arrange
    Cvs cvs = new Cvs();

    Module m = new Module();
    m.setName("foo");

    // Act
    cvs.addModule(m);

    // Assert
    List<Module> modules = cvs.getModules();
    assertEquals(1, modules.size());
    assertSame(m, modules.get(0));
  }

  /**
   * Test {@link AbstractCvsTask#getModules()}.
   * <p>
   * Method under test: {@link AbstractCvsTask#getModules()}
   */
  @Test
  public void testGetModules() {
    // Arrange, Act and Assert
    assertTrue((new Cvs()).getModules().isEmpty());
  }
}
