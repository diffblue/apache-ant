package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class WebsphereDeploymentToolDiffblueTest {
  /**
   * Test {@link WebsphereDeploymentTool#createWASClasspath()}.
   * <ul>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#createWASClasspath()}
   */
  @Test
  public void testCreateWASClasspath_thenReturnLocationFileNameIsNull() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(new TaskAdapter());

    // Act
    Path actualCreateWASClasspathResult = websphereDeploymentTool.createWASClasspath();

    // Assert
    Location location = actualCreateWASClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateWASClasspathResult.getDescription());
    assertNull(actualCreateWASClasspathResult.getProject());
    assertNull(actualCreateWASClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateWASClasspathResult.size());
    assertFalse(actualCreateWASClasspathResult.isReference());
    assertTrue(actualCreateWASClasspathResult.isEmpty());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getVendorOutputJarFile(String)}.
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile() {
    // Arrange and Act
    File actualVendorOutputJarFile = (new WebsphereDeploymentTool()).getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Codegen is {@code true}.</li>
   *   <li>Then return {@code -codegen -quiet}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolCodegenIsTrue_thenReturnCodegenQuiet() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setCodegen(true);

    // Act and Assert
    assertEquals(" -codegen -quiet", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Dbname is {@code foo}.</li>
   *   <li>Then return {@code -dbname "foo"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolDbnameIsFoo_thenReturnDbnameFoo() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setDbvendor(null);
    websphereDeploymentTool.setDbname("foo");
    websphereDeploymentTool.setDbschema(null);
    websphereDeploymentTool.setCodegen(false);
    websphereDeploymentTool.setQuiet(false);
    websphereDeploymentTool.setNovalidate(false);
    websphereDeploymentTool.setNowarn(false);
    websphereDeploymentTool.setNoinform(false);
    websphereDeploymentTool.setTrace(false);
    websphereDeploymentTool.setUse35(false);
    websphereDeploymentTool.setRmicoptions(null);

    // Act and Assert
    assertEquals(" -dbname \"foo\"", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Dbschema is {@code foo}.</li>
   *   <li>Then return {@code -dbschema "foo"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolDbschemaIsFoo_thenReturnDbschemaFoo() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setDbvendor(null);
    websphereDeploymentTool.setDbname(null);
    websphereDeploymentTool.setDbschema("foo");
    websphereDeploymentTool.setCodegen(false);
    websphereDeploymentTool.setQuiet(false);
    websphereDeploymentTool.setNovalidate(false);
    websphereDeploymentTool.setNowarn(false);
    websphereDeploymentTool.setNoinform(false);
    websphereDeploymentTool.setTrace(false);
    websphereDeploymentTool.setUse35(false);
    websphereDeploymentTool.setRmicoptions(null);

    // Act and Assert
    assertEquals(" -dbschema \"foo\"", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Dbvendor is {@code foo}.</li>
   *   <li>Then return {@code -dbvendor foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolDbvendorIsFoo_thenReturnDbvendorFoo() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setDbvendor("foo");
    websphereDeploymentTool.setDbname(null);
    websphereDeploymentTool.setDbschema(null);
    websphereDeploymentTool.setCodegen(false);
    websphereDeploymentTool.setQuiet(false);
    websphereDeploymentTool.setNovalidate(false);
    websphereDeploymentTool.setNowarn(false);
    websphereDeploymentTool.setNoinform(false);
    websphereDeploymentTool.setTrace(false);
    websphereDeploymentTool.setUse35(false);
    websphereDeploymentTool.setRmicoptions(null);

    // Act and Assert
    assertEquals(" -dbvendor foo", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Dbvendor is {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolDbvendorIsNull_thenReturnEmptyString() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setDbvendor(null);
    websphereDeploymentTool.setDbname(null);
    websphereDeploymentTool.setDbschema(null);
    websphereDeploymentTool.setCodegen(false);
    websphereDeploymentTool.setQuiet(false);
    websphereDeploymentTool.setNovalidate(false);
    websphereDeploymentTool.setNowarn(false);
    websphereDeploymentTool.setNoinform(false);
    websphereDeploymentTool.setTrace(false);
    websphereDeploymentTool.setUse35(false);
    websphereDeploymentTool.setRmicoptions(null);

    // Act and Assert
    assertEquals("", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Nowarn is {@code true}.</li>
   *   <li>Then return {@code -quiet -nowarn}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolNowarnIsTrue_thenReturnQuietNowarn() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setNowarn(true);

    // Act and Assert
    assertEquals(" -quiet -nowarn", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Rmicoptions is {@code foo}.</li>
   *   <li>Then return {@code -rmic "foo"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolRmicoptionsIsFoo_thenReturnRmicFoo() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setDbvendor(null);
    websphereDeploymentTool.setDbname(null);
    websphereDeploymentTool.setDbschema(null);
    websphereDeploymentTool.setCodegen(false);
    websphereDeploymentTool.setQuiet(false);
    websphereDeploymentTool.setNovalidate(false);
    websphereDeploymentTool.setNowarn(false);
    websphereDeploymentTool.setNoinform(false);
    websphereDeploymentTool.setTrace(false);
    websphereDeploymentTool.setUse35(false);
    websphereDeploymentTool.setRmicoptions("foo");

    // Act and Assert
    assertEquals(" -rmic \"foo\"", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Trace is {@code true}.</li>
   *   <li>Then return {@code -quiet -trace}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolTraceIsTrue_thenReturnQuietTrace() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTrace(true);

    // Act and Assert
    assertEquals(" -quiet -trace", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor) Use35 is {@code true}.</li>
   *   <li>Then return {@code -quiet -35}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentToolUse35IsTrue_thenReturnQuiet35() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setUse35(true);

    // Act and Assert
    assertEquals(" -quiet -35", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Given {@link WebsphereDeploymentTool} (default constructor).</li>
   *   <li>Then return {@code -quiet}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_givenWebsphereDeploymentTool_thenReturnQuiet() {
    // Arrange, Act and Assert
    assertEquals(" -quiet", (new WebsphereDeploymentTool()).getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Then return {@code -quiet -noinform}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_thenReturnQuietNoinform() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setNoinform(true);

    // Act and Assert
    assertEquals(" -quiet -noinform", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#getOptions()}.
   * <ul>
   *   <li>Then return {@code -quiet -novalidate}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#getOptions()}
   */
  @Test
  public void testGetOptions_thenReturnQuietNovalidate() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setNovalidate(true);

    // Act and Assert
    assertEquals(" -quiet -novalidate", websphereDeploymentTool.getOptions());
  }

  /**
   * Test {@link WebsphereDeploymentTool#validateConfigured()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#validateConfigured()}
   */
  @Test
  public void testValidateConfigured_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(task);
    websphereDeploymentTool.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> websphereDeploymentTool.validateConfigured());
  }

  /**
   * Test {@link WebsphereDeploymentTool#validateConfigured()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#validateConfigured()}
   */
  @Test
  public void testValidateConfigured_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(task);
    websphereDeploymentTool.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> websphereDeploymentTool.validateConfigured());
  }

  /**
   * Test {@link WebsphereDeploymentTool#validateConfigured()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#validateConfigured()}
   */
  @Test
  public void testValidateConfigured_givenProjectAddBuildListenerDefaultLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(task);
    websphereDeploymentTool.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> websphereDeploymentTool.validateConfigured());
  }

  /**
   * Test {@link WebsphereDeploymentTool#validateConfigured()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#validateConfigured()}
   */
  @Test
  public void testValidateConfigured_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(task);
    websphereDeploymentTool.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> websphereDeploymentTool.validateConfigured());
  }

  /**
   * Test {@link WebsphereDeploymentTool#validateConfigured()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#validateConfigured()}
   */
  @Test
  public void testValidateConfigured_givenTaskAdapterProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(task);
    websphereDeploymentTool.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> websphereDeploymentTool.validateConfigured());
  }

  /**
   * Test {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}.
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(new TaskAdapter());
    File genericJarFile = Paths
        .get(System.getProperty("java.io.tmpdir"), "Checking if websphere Jar needs to be rebuilt for jar ")
        .toFile();

    // Act and Assert
    assertTrue(websphereDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired_givenJavaLangObject_thenReturnTrue() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(task);
    File genericJarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(websphereDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired_givenProjectAddBuildListenerAntClassLoader_thenReturnTrue() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(task);
    File genericJarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(websphereDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired_givenTaskAdapterProjectIsProject_thenReturnTrue() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(task);
    File genericJarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(websphereDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebsphereDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired_thenReturnTrue() {
    // Arrange
    WebsphereDeploymentTool websphereDeploymentTool = new WebsphereDeploymentTool();
    websphereDeploymentTool.setTask(new TaskAdapter());
    File genericJarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(websphereDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link WebsphereDeploymentTool}
   *   <li>{@link WebsphereDeploymentTool#setCodegen(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setDbname(String)}
   *   <li>{@link WebsphereDeploymentTool#setDbschema(String)}
   *   <li>{@link WebsphereDeploymentTool#setDbvendor(String)}
   *   <li>{@link WebsphereDeploymentTool#setEJBdtd(String)}
   *   <li>{@link WebsphereDeploymentTool#setEjbdeploy(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setKeepgeneric(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setNewCMP(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setNoinform(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setNovalidate(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setNowarn(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setQuiet(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setRebuild(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setRmicoptions(String)}
   *   <li>{@link WebsphereDeploymentTool#setSuffix(String)}
   *   <li>{@link WebsphereDeploymentTool#setTempdir(String)}
   *   <li>{@link WebsphereDeploymentTool#setTrace(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setUse35(boolean)}
   *   <li>{@link WebsphereDeploymentTool#setWASClasspath(Path)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    WebsphereDeploymentTool actualWebsphereDeploymentTool = new WebsphereDeploymentTool();
    actualWebsphereDeploymentTool.setCodegen(true);
    actualWebsphereDeploymentTool.setDbname("Db Name");
    actualWebsphereDeploymentTool.setDbschema("Db Schema");
    actualWebsphereDeploymentTool.setDbvendor("Dbvendor");
    actualWebsphereDeploymentTool.setEJBdtd("In String");
    actualWebsphereDeploymentTool.setEjbdeploy(true);
    actualWebsphereDeploymentTool.setKeepgeneric(true);
    actualWebsphereDeploymentTool.setNewCMP(true);
    actualWebsphereDeploymentTool.setNoinform(true);
    actualWebsphereDeploymentTool.setNovalidate(true);
    actualWebsphereDeploymentTool.setNowarn(true);
    actualWebsphereDeploymentTool.setQuiet(true);
    actualWebsphereDeploymentTool.setRebuild(true);
    actualWebsphereDeploymentTool.setRmicoptions("Options");
    actualWebsphereDeploymentTool.setSuffix("In String");
    actualWebsphereDeploymentTool.setTempdir("Tempdir");
    actualWebsphereDeploymentTool.setTrace(true);
    actualWebsphereDeploymentTool.setUse35(true);
    actualWebsphereDeploymentTool.setWASClasspath(Path.systemBootClasspath);

    // Assert
    assertNull(actualWebsphereDeploymentTool.getDestDir());
    assertNull(actualWebsphereDeploymentTool.getTask());
    assertNull(actualWebsphereDeploymentTool.getConfig());
  }
}
