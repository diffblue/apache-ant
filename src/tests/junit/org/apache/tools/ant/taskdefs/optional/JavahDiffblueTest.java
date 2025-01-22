package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.Javah.ClassArgument;
import org.apache.tools.ant.taskdefs.optional.javah.ForkingJavah;
import org.apache.tools.ant.taskdefs.optional.javah.JavahAdapter;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.selectors.OrSelector;
import org.apache.tools.ant.util.facade.ImplementationSpecificArgument;
import org.junit.Test;

public class JavahDiffblueTest {
  /**
   * Test ClassArgument getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ClassArgument#ClassArgument(Javah)}
   *   <li>{@link ClassArgument#setName(String)}
   *   <li>{@link ClassArgument#getName()}
   * </ul>
   */
  @Test
  public void testClassArgumentGettersAndSetters() {
    // Arrange and Act
    ClassArgument actualClassArgument = (new Javah()).new ClassArgument();
    actualClassArgument.setName("Name");

    // Assert
    assertEquals("Name", actualClassArgument.getName());
  }

  /**
   * Test new {@link Javah} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Javah}
   */
  @Test
  public void testNewJavah() {
    // Arrange and Act
    Javah actualJavah = new Javah();

    // Assert
    assertNull(actualJavah.getDestdir());
    assertNull(actualJavah.getOutputfile());
    assertNull(actualJavah.getDescription());
    assertNull(actualJavah.getTaskName());
    assertNull(actualJavah.getTaskType());
    assertNull(actualJavah.getProject());
    assertNull(actualJavah.getOwningTarget());
    assertNull(actualJavah.getBootclasspath());
    assertNull(actualJavah.getClasspath());
    assertEquals(0, actualJavah.getClasses().length);
    assertEquals(0, actualJavah.getCurrentArgs().length);
    assertFalse(actualJavah.getForce());
    assertFalse(actualJavah.getOld());
    assertFalse(actualJavah.getStubs());
    assertFalse(actualJavah.getVerbose());
  }

  /**
   * Test {@link Javah#createClass()}.
   * <p>
   * Method under test: {@link Javah#createClass()}
   */
  @Test
  public void testCreateClass() {
    // Arrange
    Javah javah = new Javah();

    // Act and Assert
    assertNull(javah.createClass().getName());
    assertArrayEquals(new String[]{null}, javah.getClasses());
  }

  /**
   * Test {@link Javah#getClasses()}.
   * <p>
   * Method under test: {@link Javah#getClasses()}
   */
  @Test
  public void testGetClasses() {
    // Arrange
    FileSet fs = new FileSet();
    fs.addOr(new OrSelector());
    fs.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "*").toFile());
    fs.appendSelector(new ScriptSelector());

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javah javah = new Javah();
    javah.setProject(project);
    javah.addFileSet(fs);

    // Act and Assert
    assertEquals(0, javah.getClasses().length);
  }

  /**
   * Test {@link Javah#getClasses()}.
   * <p>
   * Method under test: {@link Javah#getClasses()}
   */
  @Test
  public void testGetClasses2() {
    // Arrange
    FileSet fs = new FileSet();
    fs.addOr(new OrSelector());
    fs.setFile(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    fs.appendSelector(new ScriptSelector());

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javah javah = new Javah();
    javah.setProject(project);
    javah.addFileSet(fs);

    // Act and Assert
    assertEquals(0, javah.getClasses().length);
  }

  /**
   * Test {@link Javah#getClasses()}.
   * <ul>
   *   <li>Given {@link FileSet#FileSet()} addOr {@link OrSelector} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#getClasses()}
   */
  @Test
  public void testGetClasses_givenFileSetAddOrOrSelector_thenReturnArrayLengthIsZero() {
    // Arrange
    FileSet fs = new FileSet();
    fs.addOr(new OrSelector());
    fs.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fs.appendSelector(new ScriptSelector());

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javah javah = new Javah();
    javah.setProject(project);
    javah.addFileSet(fs);

    // Act and Assert
    assertEquals(0, javah.getClasses().length);
  }

  /**
   * Test {@link Javah#getClasses()}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor) Class is {@code foo}.</li>
   *   <li>Then return array of {@link String} with {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#getClasses()}
   */
  @Test
  public void testGetClasses_givenJavahClassIsFoo_thenReturnArrayOfStringWithFoo() {
    // Arrange
    Javah javah = new Javah();
    javah.setClass("foo");

    // Act and Assert
    assertArrayEquals(new String[]{"foo"}, javah.getClasses());
  }

  /**
   * Test {@link Javah#getClasses()}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#getClasses()}
   */
  @Test
  public void testGetClasses_givenJavah_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Javah()).getClasses().length);
  }

  /**
   * Test {@link Javah#setClasspath(Path)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_givenPathWithProjectIsProjectAddFilesetFileSet() throws BuildException {
    // Arrange
    Project project = new Project();

    Path src = new Path(project);
    src.addFileset(new FileSet());

    Javah javah = new Javah();
    javah.setClasspath(src);
    Path src2 = Path.systemBootClasspath;
    src2.setProject(null);

    // Act
    javah.setClasspath(src2);

    // Assert
    assertSame(project, src2.getProject());
  }

  /**
   * Test {@link Javah#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenSystemBootClasspathProjectIsNull() {
    // Arrange
    Javah javah = new Javah();
    javah.setClasspath(new Path(null));
    Path src = Path.systemBootClasspath;
    src.setProject(null);

    // Act
    javah.setClasspath(src);

    // Assert that nothing has changed
    assertNull(src.getProject());
  }

  /**
   * Test {@link Javah#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    Javah javah = new Javah();
    Project project = new Project();
    javah.setClasspath(new Path(project));
    Path src = Path.systemBootClasspath;
    src.setProject(null);

    // Act
    javah.setClasspath(src);

    // Assert
    assertSame(project, src.getProject());
  }

  /**
   * Test {@link Javah#createClasspath()}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor).</li>
   *   <li>Then {@link Javah} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJavah_thenJavahClasspathDescriptionIsNull() {
    // Arrange
    Javah javah = new Javah();

    // Act
    Path actualCreateClasspathResult = javah.createClasspath();

    // Assert
    Path classpath = javah.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link Javah#createClasspath()}.
   * <ul>
   *   <li>Then {@link Javah} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenJavahClasspathIsSystemBootClasspath() {
    // Arrange
    Javah javah = new Javah();
    javah.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = javah.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, javah.getClasspath());
  }

  /**
   * Test {@link Javah#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenJavahClasspathIsSystemBootClasspath() {
    // Arrange
    Javah javah = new Javah();
    javah.setClasspath(Path.systemBootClasspath);

    // Act
    javah.setClasspathRef(new Reference("42"));

    // Assert that nothing has changed
    assertFalse(javah.getClasspath().isReference());
  }

  /**
   * Test {@link Javah#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor).</li>
   *   <li>Then {@link Javah} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenJavah_thenJavahClasspathDescriptionIsNull() {
    // Arrange
    Javah javah = new Javah();

    // Act
    javah.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = javah.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link Javah#setBootclasspath(Path)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setBootclasspath(Path)}
   */
  @Test
  public void testSetBootclasspath_givenPathWithProjectIsProjectAddFilesetFileSet() throws BuildException {
    // Arrange
    Project project = new Project();

    Path src = new Path(project);
    src.addFileset(new FileSet());

    Javah javah = new Javah();
    javah.setBootclasspath(src);
    Path src2 = Path.systemBootClasspath;
    src2.setProject(null);

    // Act
    javah.setBootclasspath(src2);

    // Assert
    assertSame(project, src2.getProject());
  }

  /**
   * Test {@link Javah#setBootclasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setBootclasspath(Path)}
   */
  @Test
  public void testSetBootclasspath_thenSystemBootClasspathProjectIsNull() {
    // Arrange
    Javah javah = new Javah();
    javah.setBootclasspath(new Path(null));
    Path src = Path.systemBootClasspath;
    src.setProject(null);

    // Act
    javah.setBootclasspath(src);

    // Assert that nothing has changed
    assertNull(src.getProject());
  }

  /**
   * Test {@link Javah#setBootclasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setBootclasspath(Path)}
   */
  @Test
  public void testSetBootclasspath_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    Javah javah = new Javah();
    Project project = new Project();
    javah.setBootclasspath(new Path(project));
    Path src = Path.systemBootClasspath;
    src.setProject(null);

    // Act
    javah.setBootclasspath(src);

    // Assert
    assertSame(project, src.getProject());
  }

  /**
   * Test {@link Javah#createBootclasspath()}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor).</li>
   *   <li>Then {@link Javah} (default constructor) Bootclasspath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_givenJavah_thenJavahBootclasspathDescriptionIsNull() {
    // Arrange
    Javah javah = new Javah();

    // Act
    Path actualCreateBootclasspathResult = javah.createBootclasspath();

    // Assert
    Path bootclasspath = javah.getBootclasspath();
    assertNull(bootclasspath.getDescription());
    assertNull(actualCreateBootclasspathResult.getProject());
    assertNull(bootclasspath.getProject());
    assertNull(bootclasspath.getRefid());
    assertEquals(0, bootclasspath.size());
    assertFalse(bootclasspath.isReference());
    assertTrue(bootclasspath.isEmpty());
  }

  /**
   * Test {@link Javah#createBootclasspath()}.
   * <ul>
   *   <li>Then {@link Javah} (default constructor) Bootclasspath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_thenJavahBootclasspathIsSystemBootClasspath() {
    // Arrange
    Javah javah = new Javah();
    javah.setBootclasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedBootclasspath = javah.createBootclasspath().systemBootClasspath;
    assertSame(expectedBootclasspath, javah.getBootclasspath());
  }

  /**
   * Test {@link Javah#setBootClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor) Bootclasspath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setBootClasspathRef(Reference)}
   */
  @Test
  public void testSetBootClasspathRef_givenJavahBootclasspathIsSystemBootClasspath() {
    // Arrange
    Javah javah = new Javah();
    javah.setBootclasspath(Path.systemBootClasspath);

    // Act
    javah.setBootClasspathRef(new Reference("42"));

    // Assert that nothing has changed
    assertFalse(javah.getBootclasspath().isReference());
  }

  /**
   * Test {@link Javah#setBootClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor).</li>
   *   <li>Then {@link Javah} (default constructor) Bootclasspath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#setBootClasspathRef(Reference)}
   */
  @Test
  public void testSetBootClasspathRef_givenJavah_thenJavahBootclasspathDescriptionIsNull() {
    // Arrange
    Javah javah = new Javah();

    // Act
    javah.setBootClasspathRef(new Reference("42"));

    // Assert
    Path bootclasspath = javah.getBootclasspath();
    assertNull(bootclasspath.getDescription());
    assertNull(bootclasspath.getProject());
    assertNull(bootclasspath.getRefid());
    assertFalse(bootclasspath.isReference());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Javah#setClass(String)}
   *   <li>{@link Javah#setDestdir(File)}
   *   <li>{@link Javah#setForce(boolean)}
   *   <li>{@link Javah#setOld(boolean)}
   *   <li>{@link Javah#setOutputFile(File)}
   *   <li>{@link Javah#setStubs(boolean)}
   *   <li>{@link Javah#setVerbose(boolean)}
   *   <li>{@link Javah#getBootclasspath()}
   *   <li>{@link Javah#getClasspath()}
   *   <li>{@link Javah#getDestdir()}
   *   <li>{@link Javah#getForce()}
   *   <li>{@link Javah#getOld()}
   *   <li>{@link Javah#getOutputfile()}
   *   <li>{@link Javah#getStubs()}
   *   <li>{@link Javah#getVerbose()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Javah javah = new Javah();

    // Act
    javah.setClass("Cls");
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    javah.setDestdir(destDir);
    javah.setForce(true);
    javah.setOld(true);
    File outputFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    javah.setOutputFile(outputFile);
    javah.setStubs(true);
    javah.setVerbose(true);
    Path actualBootclasspath = javah.getBootclasspath();
    Path actualClasspath = javah.getClasspath();
    File actualDestdir = javah.getDestdir();
    boolean actualForce = javah.getForce();
    boolean actualOld = javah.getOld();
    File actualOutputfile = javah.getOutputfile();
    boolean actualStubs = javah.getStubs();

    // Assert
    assertNull(actualBootclasspath);
    assertNull(actualClasspath);
    assertTrue(actualForce);
    assertTrue(actualOld);
    assertTrue(actualStubs);
    assertTrue(javah.getVerbose());
    assertSame(destDir, actualDestdir);
    assertSame(outputFile, actualOutputfile);
  }

  /**
   * Test {@link Javah#createArg()}.
   * <p>
   * Method under test: {@link Javah#createArg()}
   */
  @Test
  public void testCreateArg() {
    // Arrange and Act
    ImplementationSpecificArgument actualCreateArgResult = (new Javah()).createArg();

    // Assert
    assertNull(actualCreateArgResult.getParts());
    Location location = actualCreateArgResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateArgResult.getDescription());
    assertNull(actualCreateArgResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Javah#getCurrentArgs()}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#getCurrentArgs()}
   */
  @Test
  public void testGetCurrentArgs_givenJavah() {
    // Arrange, Act and Assert
    assertEquals(0, (new Javah()).getCurrentArgs().length);
  }

  /**
   * Test {@link Javah#getCurrentArgs()}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor) Implementation is {@code Impl}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#getCurrentArgs()}
   */
  @Test
  public void testGetCurrentArgs_givenJavahImplementationIsImpl() {
    // Arrange
    Javah javah = new Javah();
    javah.setImplementation("Impl");

    // Act and Assert
    assertEquals(0, javah.getCurrentArgs().length);
  }

  /**
   * Test {@link Javah#createImplementationClasspath()}.
   * <p>
   * Method under test: {@link Javah#createImplementationClasspath()}
   */
  @Test
  public void testCreateImplementationClasspath() {
    // Arrange and Act
    Path actualCreateImplementationClasspathResult = (new Javah()).createImplementationClasspath();

    // Assert
    Location location = actualCreateImplementationClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateImplementationClasspathResult.getDescription());
    assertNull(actualCreateImplementationClasspathResult.getProject());
    assertNull(actualCreateImplementationClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateImplementationClasspathResult.size());
    assertFalse(actualCreateImplementationClasspathResult.isReference());
    assertTrue(actualCreateImplementationClasspathResult.isEmpty());
  }

  /**
   * Test {@link Javah#add(JavahAdapter)}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor) add {@link ForkingJavah} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#add(JavahAdapter)}
   */
  @Test
  public void testAdd_givenJavahAddForkingJavah_thenThrowBuildException() {
    // Arrange
    Javah javah = new Javah();
    javah.add(new ForkingJavah());

    // Act and Assert
    assertThrows(BuildException.class, () -> javah.add(new ForkingJavah()));
  }

  /**
   * Test {@link Javah#execute()}.
   * <p>
   * Method under test: {@link Javah#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Javah javah = new Javah();
    javah.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "last").toFile());
    javah.addFileSet(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> javah.execute());
  }

  /**
   * Test {@link Javah#execute()}.
   * <ul>
   *   <li>Given {@link Javah} (default constructor) Class is {@code last}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#execute()}
   */
  @Test
  public void testExecute_givenJavahClassIsLast_thenThrowBuildException() throws BuildException {
    // Arrange
    Javah javah = new Javah();
    javah.setClass("last");
    javah.addFileSet(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> javah.execute());
  }

  /**
   * Test {@link Javah#logAndAddFiles(Commandline)}.
   * <ul>
   *   <li>Then array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#logAndAddFiles(Commandline)}
   */
  @Test
  public void testLogAndAddFiles_thenArrayLengthIsZero() {
    // Arrange
    Javah javah = new Javah();
    Commandline cmd = new Commandline("Compilation ");

    // Act
    javah.logAndAddFiles(cmd);

    // Assert that nothing has changed
    assertEquals(0, cmd.getArguments().length);
    assertEquals(1, cmd.size());
    assertEquals(1, cmd.getCommandline().length);
    assertFalse(cmd.iterator().hasNext());
  }

  /**
   * Test {@link Javah#logAndAddFilesToCompile(Commandline)}.
   * <ul>
   *   <li>Then array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javah#logAndAddFilesToCompile(Commandline)}
   */
  @Test
  public void testLogAndAddFilesToCompile_thenArrayLengthIsZero() {
    // Arrange
    Javah javah = new Javah();
    Commandline cmd = new Commandline("Compilation ");

    // Act
    javah.logAndAddFilesToCompile(cmd);

    // Assert that nothing has changed
    assertEquals(0, cmd.getArguments().length);
    assertEquals(1, cmd.size());
    assertEquals(1, cmd.getCommandline().length);
    assertFalse(cmd.iterator().hasNext());
  }
}
