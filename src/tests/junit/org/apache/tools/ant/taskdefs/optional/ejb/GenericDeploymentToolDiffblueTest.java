package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Hashtable;
import javax.xml.parsers.SAXParser;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.ExecutorTest;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.optional.ejb.EjbJar.Config;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class GenericDeploymentToolDiffblueTest {
  /**
   * Test {@link GenericDeploymentTool#createClasspath()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenTaskAdapterProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    Project project = new Project();
    task.setProject(project);

    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(task);

    // Act and Assert
    assertSame(project, genericDeploymentTool.createClasspath().getProject());
  }

  /**
   * Test {@link GenericDeploymentTool#createClasspath()}.
   * <ul>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnLocationFileNameIsNull() {
    // Arrange
    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(new TaskAdapter());

    // Act
    Path actualCreateClasspathResult = genericDeploymentTool.createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link GenericDeploymentTool#getLocation()}.
   * <ul>
   *   <li>Then {@link GenericDeploymentTool} (default constructor) Task {@link TaskAdapter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getLocation()}
   */
  @Test
  public void testGetLocation_thenGenericDeploymentToolTaskTaskAdapter() {
    // Arrange
    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(new TaskAdapter());

    // Act
    Location actualLocation = genericDeploymentTool.getLocation();

    // Assert
    Task task = genericDeploymentTool.getTask();
    assertTrue(task instanceof TaskAdapter);
    Location location = actualLocation.UNKNOWN_LOCATION;
    assertSame(location, task.getLocation());
    assertSame(location, actualLocation);
  }

  /**
   * Test {@link GenericDeploymentTool#configure(Config)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#configure(Config)}
   */
  @Test
  public void testConfigure_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(GenericDeploymentTool.ANALYZER_NONE, typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(task);
    Config config = new Config();

    // Act
    genericDeploymentTool.configure(config);

    // Assert
    assertNull(genericDeploymentTool.getCombinedClasspath());
    assertSame(config, genericDeploymentTool.getConfig());
  }

  /**
   * Test {@link GenericDeploymentTool#configure(Config)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#configure(Config)}
   */
  @Test
  public void testConfigure_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(task);
    Config config = new Config();

    // Act
    genericDeploymentTool.configure(config);

    // Assert
    assertNull(genericDeploymentTool.getCombinedClasspath());
    assertSame(config, genericDeploymentTool.getConfig());
  }

  /**
   * Test {@link GenericDeploymentTool#configure(Config)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#configure(Config)}
   */
  @Test
  public void testConfigure_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(task);
    Config config = new Config();

    // Act
    genericDeploymentTool.configure(config);

    // Assert
    assertNull(genericDeploymentTool.getCombinedClasspath());
    assertSame(config, genericDeploymentTool.getConfig());
  }

  /**
   * Test {@link GenericDeploymentTool#configure(Config)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#configure(Config)}
   */
  @Test
  public void testConfigure_givenProjectAddBuildListenerExecutorTest() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new ExecutorTest());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(task);
    Config config = new Config();

    // Act
    genericDeploymentTool.configure(config);

    // Assert
    assertNull(genericDeploymentTool.getCombinedClasspath());
    assertSame(config, genericDeploymentTool.getConfig());
  }

  /**
   * Test {@link GenericDeploymentTool#configure(Config)}.
   * <ul>
   *   <li>Then {@link GenericDeploymentTool} (default constructor) CombinedClasspath is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#configure(Config)}
   */
  @Test
  public void testConfigure_thenGenericDeploymentToolCombinedClasspathIsNull() {
    // Arrange
    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(new TaskAdapter());
    Config config = new Config();

    // Act
    genericDeploymentTool.configure(config);

    // Assert
    assertNull(genericDeploymentTool.getCombinedClasspath());
    assertSame(config, genericDeploymentTool.getConfig());
  }

  /**
   * Test {@link GenericDeploymentTool#configure(Config)}.
   * <ul>
   *   <li>Then {@link GenericDeploymentTool} (default constructor) CombinedClasspath is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#configure(Config)}
   */
  @Test
  public void testConfigure_thenGenericDeploymentToolCombinedClasspathIsNull2() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(task);
    Config config = new Config();

    // Act
    genericDeploymentTool.configure(config);

    // Assert
    assertNull(genericDeploymentTool.getCombinedClasspath());
    assertSame(config, genericDeploymentTool.getConfig());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return Name is {@code Base Name.jar}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_givenJavaLangObject_thenReturnNameIsBaseNameJar() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_givenProjectAddBuildListenerExecutorTest() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new ExecutorTest());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_givenTaskAdapterProjectIsProject() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Then return Absolute.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_thenReturnAbsolute() {
    // Arrange
    JbossDeploymentTool jbossDeploymentTool = new JbossDeploymentTool();
    jbossDeploymentTool.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    File actualVendorOutputJarFile = jbossDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertTrue(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Then return Name is {@code Base Name-ejb.jar}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_thenReturnNameIsBaseNameEjbJar() {
    // Arrange and Act
    File actualVendorOutputJarFile = (new BorlandDeploymentTool()).getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name-ejb.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Then return Name is {@code Base Name-generic.jar}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_thenReturnNameIsBaseNameGenericJar() {
    // Arrange and Act
    File actualVendorOutputJarFile = (new GenericDeploymentTool()).getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name-generic.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Then return Name is {@code Base Name.jar}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_thenReturnNameIsBaseNameJar() {
    // Arrange
    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(new TaskAdapter());

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link GenericDeploymentTool#needToRebuild(Hashtable, File)}.
   * <ul>
   *   <li>Given {@link GenericDeploymentTool} (default constructor).</li>
   *   <li>When {@link Hashtable#Hashtable()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#needToRebuild(Hashtable, File)}
   */
  @Test
  public void testNeedToRebuild_givenGenericDeploymentTool_whenHashtable_thenReturnFalse() {
    // Arrange
    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    Hashtable<String, File> ejbFiles = new Hashtable<>();

    // Act and Assert
    assertFalse(genericDeploymentTool.needToRebuild(ejbFiles,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link GenericDeploymentTool#needToRebuild(Hashtable, File)}.
   * <ul>
   *   <li>Given Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#needToRebuild(Hashtable, File)}
   */
  @Test
  public void testNeedToRebuild_givenPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToFile() {
    // Arrange
    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();

    Hashtable<String, File> ejbFiles = new Hashtable<>();
    ejbFiles.put("foo", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertFalse(genericDeploymentTool.needToRebuild(ejbFiles,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link GenericDeploymentTool#needToRebuild(Hashtable, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code Build needed because} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#needToRebuild(Hashtable, File)}
   */
  @Test
  public void testNeedToRebuild_whenPropertyIsJavaIoTmpdirIsBuildNeededBecauseToFile() {
    // Arrange
    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(new TaskAdapter());

    Hashtable<String, File> ejbFiles = new Hashtable<>();
    ejbFiles.put("foo", Paths.get(System.getProperty("java.io.tmpdir"), "42").toFile());

    // Act and Assert
    assertTrue(genericDeploymentTool.needToRebuild(ejbFiles,
        Paths.get(System.getProperty("java.io.tmpdir"), "Build needed because ").toFile()));
  }

  /**
   * Test {@link GenericDeploymentTool#validateConfigured()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericDeploymentTool#validateConfigured()}
   */
  @Test
  public void testValidateConfigured_thenThrowBuildException() throws BuildException {
    // Arrange
    GenericDeploymentTool genericDeploymentTool = new GenericDeploymentTool();
    genericDeploymentTool.setTask(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> genericDeploymentTool.validateConfigured());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link GenericDeploymentTool}
   *   <li>{@link GenericDeploymentTool#setClasspath(Path)}
   *   <li>{@link GenericDeploymentTool#setDestdir(File)}
   *   <li>{@link GenericDeploymentTool#setGenericJarSuffix(String)}
   *   <li>{@link GenericDeploymentTool#setTask(Task)}
   *   <li>{@link GenericDeploymentTool#addVendorFiles(Hashtable, String)}
   *   <li>{@link GenericDeploymentTool#checkConfiguration(String, SAXParser)}
   *   <li>{@link GenericDeploymentTool#registerKnownDTDs(DescriptorHandler)}
   *   <li>{@link GenericDeploymentTool#getConfig()}
   *   <li>{@link GenericDeploymentTool#getDestDir()}
   *   <li>{@link GenericDeploymentTool#getTask()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws BuildException {
    // Arrange and Act
    GenericDeploymentTool actualGenericDeploymentTool = new GenericDeploymentTool();
    actualGenericDeploymentTool.setClasspath(Path.systemBootClasspath);
    File inDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualGenericDeploymentTool.setDestdir(inDir);
    actualGenericDeploymentTool.setGenericJarSuffix("In String");
    TaskAdapter task = new TaskAdapter();
    actualGenericDeploymentTool.setTask(task);
    actualGenericDeploymentTool.addVendorFiles(new Hashtable<>(), "Dd Prefix");
    actualGenericDeploymentTool.checkConfiguration("foo.txt", null);
    TaskAdapter task2 = new TaskAdapter();
    actualGenericDeploymentTool.registerKnownDTDs(
        new DescriptorHandler(task2, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    Config actualConfig = actualGenericDeploymentTool.getConfig();
    File actualDestDir = actualGenericDeploymentTool.getDestDir();

    // Assert
    assertNull(actualConfig);
    assertSame(task, actualGenericDeploymentTool.getTask());
    assertSame(inDir, actualDestDir);
  }
}
