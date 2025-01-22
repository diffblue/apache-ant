package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.filters.ClassConstants;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.filters.TokenFilter;
import org.apache.tools.ant.filters.TokenFilter.ContainsRegex;
import org.apache.tools.ant.taskdefs.Rmic.ImplementationSpecificArgument;
import org.apache.tools.ant.taskdefs.rmic.ForkingSunRmic;
import org.apache.tools.ant.taskdefs.rmic.RmicAdapter;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.resources.FileResourceIterator;
import org.apache.tools.ant.util.ChainedMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class RmicDiffblueTest {
  /**
   * Test ImplementationSpecificArgument {@link ImplementationSpecificArgument#ImplementationSpecificArgument(Rmic)}.
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#ImplementationSpecificArgument(Rmic)}
   */
  @Test
  public void testImplementationSpecificArgumentNewImplementationSpecificArgument() {
    // Arrange and Act
    ImplementationSpecificArgument actualImplementationSpecificArgument = (new Rmic()).new ImplementationSpecificArgument();

    // Assert
    assertNull(actualImplementationSpecificArgument.getParts());
    Location location = actualImplementationSpecificArgument.getLocation();
    assertNull(location.getFileName());
    assertNull(actualImplementationSpecificArgument.getDescription());
    assertNull(actualImplementationSpecificArgument.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test new {@link Rmic} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Rmic}
   */
  @Test
  public void testNewRmic() {
    // Arrange and Act
    Rmic actualRmic = new Rmic();

    // Assert
    assertNull(actualRmic.getBase());
    assertNull(actualRmic.getDestdir());
    assertNull(actualRmic.getOutputDir());
    assertNull(actualRmic.getSourceBase());
    assertNull(actualRmic.getLoader());
    assertNull(actualRmic.getDescription());
    assertNull(actualRmic.getTaskName());
    assertNull(actualRmic.getTaskType());
    assertNull(actualRmic.getClassname());
    assertNull(actualRmic.getExecutable());
    assertNull(actualRmic.getIdlopts());
    assertNull(actualRmic.getIiopopts());
    assertNull(actualRmic.getStubVersion());
    assertNull(actualRmic.getProject());
    assertNull(actualRmic.getOwningTarget());
    assertNull(actualRmic.getClasspath());
    assertNull(actualRmic.getExtdirs());
    assertFalse(actualRmic.hasSelectors());
    assertFalse(actualRmic.getDebug());
    assertFalse(actualRmic.getFiltering());
    assertFalse(actualRmic.getIdl());
    assertFalse(actualRmic.getIiop());
    assertFalse(actualRmic.getIncludejavaruntime());
    assertFalse(actualRmic.getVerify());
    Vector<String> compileList = actualRmic.getCompileList();
    assertTrue(compileList.isEmpty());
    assertTrue(actualRmic.getIncludeantruntime());
    assertSame(compileList, actualRmic.getFileList());
  }

  /**
   * Test {@link Rmic#getOutputDir()}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getOutputDir()}
   */
  @Test
  public void testGetOutputDir_givenRmic_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Rmic()).getOutputDir());
  }

  /**
   * Test {@link Rmic#getOutputDir()}.
   * <ul>
   *   <li>Then return Name is {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getOutputDir()}
   */
  @Test
  public void testGetOutputDir_thenReturnNameIsNullFile() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setDestdir(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    File actualOutputDir = rmic.getOutputDir();

    // Assert
    assertEquals("NULL_FILE", actualOutputDir.getName());
    assertTrue(actualOutputDir.isAbsolute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Rmic#setBase(File)}
   *   <li>{@link Rmic#setClassname(String)}
   *   <li>{@link Rmic#setDebug(boolean)}
   *   <li>{@link Rmic#setDestdir(File)}
   *   <li>{@link Rmic#setExecutable(String)}
   *   <li>{@link Rmic#setFiltering(boolean)}
   *   <li>{@link Rmic#setIdl(boolean)}
   *   <li>{@link Rmic#setIdlopts(String)}
   *   <li>{@link Rmic#setIiop(boolean)}
   *   <li>{@link Rmic#setIiopopts(String)}
   *   <li>{@link Rmic#setIncludeantruntime(boolean)}
   *   <li>{@link Rmic#setIncludejavaruntime(boolean)}
   *   <li>{@link Rmic#setListfiles(boolean)}
   *   <li>{@link Rmic#setSourceBase(File)}
   *   <li>{@link Rmic#setStubVersion(String)}
   *   <li>{@link Rmic#setVerify(boolean)}
   *   <li>{@link Rmic#getBase()}
   *   <li>{@link Rmic#getClassname()}
   *   <li>{@link Rmic#getClasspath()}
   *   <li>{@link Rmic#getCompileList()}
   *   <li>{@link Rmic#getDebug()}
   *   <li>{@link Rmic#getDestdir()}
   *   <li>{@link Rmic#getExecutable()}
   *   <li>{@link Rmic#getExtdirs()}
   *   <li>{@link Rmic#getFileList()}
   *   <li>{@link Rmic#getFiltering()}
   *   <li>{@link Rmic#getIdl()}
   *   <li>{@link Rmic#getIdlopts()}
   *   <li>{@link Rmic#getIiop()}
   *   <li>{@link Rmic#getIiopopts()}
   *   <li>{@link Rmic#getIncludeantruntime()}
   *   <li>{@link Rmic#getIncludejavaruntime()}
   *   <li>{@link Rmic#getLoader()}
   *   <li>{@link Rmic#getSourceBase()}
   *   <li>{@link Rmic#getStubVersion()}
   *   <li>{@link Rmic#getVerify()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.setBase(Copy.NULL_FILE_PLACEHOLDER);
    rmic.setClassname("Classname");
    rmic.setDebug(true);
    rmic.setDestdir(Copy.NULL_FILE_PLACEHOLDER);
    rmic.setExecutable("Ex");
    rmic.setFiltering(true);
    rmic.setIdl(true);
    rmic.setIdlopts("Idl Opts");
    rmic.setIiop(true);
    rmic.setIiopopts("Iiop Opts");
    rmic.setIncludeantruntime(true);
    rmic.setIncludejavaruntime(true);
    rmic.setListfiles(true);
    File sourceBase = Copy.NULL_FILE_PLACEHOLDER;
    rmic.setSourceBase(sourceBase);
    rmic.setStubVersion("1.0.2");
    rmic.setVerify(true);
    File actualBase = rmic.getBase();
    String actualClassname = rmic.getClassname();
    Path actualClasspath = rmic.getClasspath();
    Vector<String> actualCompileList = rmic.getCompileList();
    boolean actualDebug = rmic.getDebug();
    File actualDestdir = rmic.getDestdir();
    String actualExecutable = rmic.getExecutable();
    Path actualExtdirs = rmic.getExtdirs();
    Vector<String> actualFileList = rmic.getFileList();
    boolean actualFiltering = rmic.getFiltering();
    boolean actualIdl = rmic.getIdl();
    String actualIdlopts = rmic.getIdlopts();
    boolean actualIiop = rmic.getIiop();
    String actualIiopopts = rmic.getIiopopts();
    boolean actualIncludeantruntime = rmic.getIncludeantruntime();
    boolean actualIncludejavaruntime = rmic.getIncludejavaruntime();
    ClassLoader actualLoader = rmic.getLoader();
    File actualSourceBase = rmic.getSourceBase();
    String actualStubVersion = rmic.getStubVersion();

    // Assert
    assertEquals("1.0.2", actualStubVersion);
    assertEquals("Classname", actualClassname);
    assertEquals("Ex", actualExecutable);
    assertEquals("Idl Opts", actualIdlopts);
    assertEquals("Iiop Opts", actualIiopopts);
    assertNull(actualLoader);
    assertNull(actualClasspath);
    assertNull(actualExtdirs);
    assertTrue(actualDebug);
    assertTrue(actualFiltering);
    assertTrue(actualIdl);
    assertTrue(actualIiop);
    assertTrue(actualIncludeantruntime);
    assertTrue(actualIncludejavaruntime);
    assertTrue(rmic.getVerify());
    assertSame(actualCompileList, actualFileList);
    assertSame(sourceBase, actualBase);
    assertSame(sourceBase, actualDestdir);
    assertSame(sourceBase, actualSourceBase);
  }

  /**
   * Test {@link Rmic#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setClasspath(Path.systemBootClasspath);

    // Act
    rmic.setClasspath(null);

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#setClasspath(Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenPathWithProjectIsNull_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setClasspath(Path.systemBootClasspath);

    // Act
    rmic.setClasspath(new Path(null));

    // Assert
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#createClasspath()}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>Then {@link Rmic} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenRmic_thenRmicClasspathDescriptionIsNull() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    Path actualCreateClasspathResult = rmic.createClasspath();

    // Assert
    Path classpath = rmic.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link Rmic#createClasspath()}.
   * <ul>
   *   <li>Then {@link Rmic} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenRmicClasspathIsSystemBootClasspath() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = rmic.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, rmic.getClasspath());
  }

  /**
   * Test {@link Rmic#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>Then {@link Rmic} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenRmic_thenRmicClasspathProjectIsNull() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = rmic.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link Rmic#setExtdirs(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#setExtdirs(Path)}
   */
  @Test
  public void testSetExtdirs_whenNull_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setExtdirs(Path.systemBootClasspath);

    // Act
    rmic.setExtdirs(null);

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#setExtdirs(Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#setExtdirs(Path)}
   */
  @Test
  public void testSetExtdirs_whenPathWithProjectIsNull_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setExtdirs(Path.systemBootClasspath);

    // Act
    rmic.setExtdirs(new Path(null));

    // Assert
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#createExtdirs()}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>Then {@link Rmic} (default constructor) Extdirs Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#createExtdirs()}
   */
  @Test
  public void testCreateExtdirs_givenRmic_thenRmicExtdirsDescriptionIsNull() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    Path actualCreateExtdirsResult = rmic.createExtdirs();

    // Assert
    Path extdirs = rmic.getExtdirs();
    assertNull(extdirs.getDescription());
    assertNull(actualCreateExtdirsResult.getProject());
    assertNull(extdirs.getProject());
    assertNull(extdirs.getRefid());
    assertEquals(0, extdirs.size());
    assertFalse(extdirs.isReference());
    assertTrue(extdirs.isEmpty());
  }

  /**
   * Test {@link Rmic#createExtdirs()}.
   * <ul>
   *   <li>Then {@link Rmic} (default constructor) Extdirs is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#createExtdirs()}
   */
  @Test
  public void testCreateExtdirs_thenRmicExtdirsIsSystemBootClasspath() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setExtdirs(Path.systemBootClasspath);

    // Act and Assert
    Path expectedExtdirs = rmic.createExtdirs().systemBootClasspath;
    assertSame(expectedExtdirs, rmic.getExtdirs());
  }

  /**
   * Test {@link Rmic#getCompiler()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code default}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getCompiler()}
   */
  @Test
  public void testGetCompiler_givenJavaLangObject_thenReturnDefault() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertEquals("default", rmic.getCompiler());
  }

  /**
   * Test {@link Rmic#getCompiler()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code default}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getCompiler()}
   */
  @Test
  public void testGetCompiler_givenProjectAddBuildListenerAntClassLoader_thenReturnDefault() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertEquals("default", rmic.getCompiler());
  }

  /**
   * Test {@link Rmic#getCompiler()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getCompiler()}
   */
  @Test
  public void testGetCompiler_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertEquals("default", rmic.getCompiler());
  }

  /**
   * Test {@link Rmic#getCompiler()}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code default}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getCompiler()}
   */
  @Test
  public void testGetCompiler_givenRmicProjectIsProject_thenReturnDefault() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setProject(new Project());

    // Act and Assert
    assertEquals("default", rmic.getCompiler());
  }

  /**
   * Test {@link Rmic#createCompilerArg()}.
   * <p>
   * Method under test: {@link Rmic#createCompilerArg()}
   */
  @Test
  public void testCreateCompilerArg() {
    // Arrange and Act
    ImplementationSpecificArgument actualCreateCompilerArgResult = (new Rmic()).createCompilerArg();

    // Assert
    assertNull(actualCreateCompilerArgResult.getParts());
    Location location = actualCreateCompilerArgResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateCompilerArgResult.getDescription());
    assertNull(actualCreateCompilerArgResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Rmic#getCurrentCompilerArgs()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getCurrentCompilerArgs()}
   */
  @Test
  public void testGetCurrentCompilerArgs_givenJavaLangObject_thenReturnArrayLengthIsZero() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertEquals(0, rmic.getCurrentCompilerArgs().length);
  }

  /**
   * Test {@link Rmic#getCurrentCompilerArgs()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getCurrentCompilerArgs()}
   */
  @Test
  public void testGetCurrentCompilerArgs_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertEquals(0, rmic.getCurrentCompilerArgs().length);
  }

  /**
   * Test {@link Rmic#getCurrentCompilerArgs()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getCurrentCompilerArgs()}
   */
  @Test
  public void testGetCurrentCompilerArgs_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertEquals(0, rmic.getCurrentCompilerArgs().length);
  }

  /**
   * Test {@link Rmic#getCurrentCompilerArgs()}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getCurrentCompilerArgs()}
   */
  @Test
  public void testGetCurrentCompilerArgs_givenRmicProjectIsProject_thenReturnArrayLengthIsZero() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setProject(new Project());

    // Act and Assert
    assertEquals(0, rmic.getCurrentCompilerArgs().length);
  }

  /**
   * Test {@link Rmic#createCompilerClasspath()}.
   * <p>
   * Method under test: {@link Rmic#createCompilerClasspath()}
   */
  @Test
  public void testCreateCompilerClasspath() {
    // Arrange and Act
    Path actualCreateCompilerClasspathResult = (new Rmic()).createCompilerClasspath();

    // Assert
    Location location = actualCreateCompilerClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateCompilerClasspathResult.getDescription());
    assertNull(actualCreateCompilerClasspathResult.getProject());
    assertNull(actualCreateCompilerClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateCompilerClasspathResult.size());
    assertFalse(actualCreateCompilerClasspathResult.isReference());
    assertTrue(actualCreateCompilerClasspathResult.isEmpty());
  }

  /**
   * Test {@link Rmic#add(RmicAdapter)} with {@code adapter}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) add {@link ForkingSunRmic} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#add(RmicAdapter)}
   */
  @Test
  public void testAddWithAdapter_givenRmicAddForkingSunRmic_thenThrowBuildException() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.add(new ForkingSunRmic());

    // Act and Assert
    assertThrows(BuildException.class, () -> rmic.add(new ForkingSunRmic()));
  }

  /**
   * Test {@link Rmic#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("setProject", typeClass);
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);
    rmic.add(new ForkingSunRmic());
    rmic.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    rmic.execute();

    // Assert
    FileSet implicitFileSet = rmic.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    DirectoryScanner directoryScanner = implicitFileSet.getDirectoryScanner();
    assertEquals(0, directoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, directoryScanner.getIncludedFiles().length);
    assertEquals(0, directoryScanner.getNotFollowedSymlinks().length);
    assertEquals(1, directoryScanner.getIncludedDirsCount());
    assertEquals(1, directoryScanner.getIncludedDirectories().length);
    assertFalse(iteratorResult.hasNext());
    assertTrue(directoryScanner.isCaseSensitive());
    assertTrue(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
  }

  /**
   * Test {@link Rmic#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);
    rmic.add(new ForkingSunRmic());
    rmic.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    rmic.execute();

    // Assert
    FileSet implicitFileSet = rmic.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    DirectoryScanner directoryScanner = implicitFileSet.getDirectoryScanner();
    assertEquals(0, directoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, directoryScanner.getIncludedFiles().length);
    assertEquals(0, directoryScanner.getNotFollowedSymlinks().length);
    assertEquals(1, directoryScanner.getIncludedDirsCount());
    assertEquals(1, directoryScanner.getIncludedDirectories().length);
    assertFalse(iteratorResult.hasNext());
    assertTrue(directoryScanner.isCaseSensitive());
    assertTrue(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
  }

  /**
   * Test {@link Rmic#execute()}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) Base is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#execute()}
   */
  @Test
  public void testExecute_givenRmicBaseIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setBase(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> rmic.execute());
  }

  /**
   * Test {@link Rmic#execute()}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) Destdir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#execute()}
   */
  @Test
  public void testExecute_givenRmicDestdirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setDestdir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> rmic.execute());
  }

  /**
   * Test {@link Rmic#execute()}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#execute()}
   */
  @Test
  public void testExecute_givenRmic_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Rmic()).execute());
  }

  /**
   * Test {@link Rmic#execute()}.
   * <ul>
   *   <li>Then {@link Rmic} (default constructor) ImplicitFileSet iterator {@link FileResourceIterator}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#execute()}
   */
  @Test
  public void testExecute_thenRmicImplicitFileSetIteratorFileResourceIterator() throws BuildException {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setProject(new Project());
    rmic.add(new ForkingSunRmic());
    rmic.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    rmic.execute();

    // Assert
    FileSet implicitFileSet = rmic.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    DirectoryScanner directoryScanner = implicitFileSet.getDirectoryScanner();
    assertEquals(0, directoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, directoryScanner.getIncludedFiles().length);
    assertEquals(0, directoryScanner.getNotFollowedSymlinks().length);
    assertEquals(1, directoryScanner.getIncludedDirsCount());
    assertEquals(1, directoryScanner.getIncludedDirectories().length);
    assertFalse(iteratorResult.hasNext());
    assertTrue(directoryScanner.isCaseSensitive());
    assertTrue(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
  }

  /**
   * Test {@link Rmic#execute()}.
   * <ul>
   *   <li>Then {@link Rmic} (default constructor) ImplicitFileSet iterator {@link FileResourceIterator}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#execute()}
   */
  @Test
  public void testExecute_thenRmicImplicitFileSetIteratorFileResourceIterator2() throws BuildException {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setProject(new Project());
    rmic.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    rmic.execute();

    // Assert
    FileSet implicitFileSet = rmic.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    DirectoryScanner directoryScanner = implicitFileSet.getDirectoryScanner();
    assertEquals(0, directoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, directoryScanner.getIncludedFiles().length);
    assertEquals(0, directoryScanner.getNotFollowedSymlinks().length);
    assertEquals(1, directoryScanner.getIncludedDirsCount());
    assertEquals(1, directoryScanner.getIncludedDirectories().length);
    assertFalse(iteratorResult.hasNext());
    assertTrue(directoryScanner.isCaseSensitive());
    assertTrue(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   *   <li>When {@link FilterMapper} (default constructor) addClassConstants {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenClassConstants_whenFilterMapperAddClassConstantsClassConstants() {
    // Arrange
    Rmic rmic = new Rmic();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, mapper);

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor).</li>
   *   <li>Then {@link FilterMapper} (default constructor) FilterReaders size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenContainsRegex_thenFilterMapperFilterReadersSizeIsOne() {
    // Arrange
    Rmic rmic = new Rmic();

    FilterMapper mapper = new FilterMapper();
    mapper.addContainsRegex(new ContainsRegex());

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, mapper);

    // Assert that nothing has changed
    Vector<Object> filterReaders = mapper.getFilterReaders();
    assertEquals(1, filterReaders.size());
    assertTrue(filterReaders.get(0) instanceof ContainsRegex);
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenExpandProperties() {
    // Arrange
    Rmic rmic = new Rmic();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, mapper);

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenExpandProperties2() {
    // Arrange
    Rmic rmic = new Rmic();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());
    mapper.addContainsRegex(new ContainsRegex());

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, mapper);

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then {@link FilterMapper} (default constructor) FilterReaders first Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenProject_thenFilterMapperFilterReadersFirstProjectIsProject() {
    // Arrange
    Rmic rmic = new Rmic();

    FilterMapper mapper = new FilterMapper();
    Project project = new Project();
    mapper.setProject(project);
    mapper.addContainsRegex(new ContainsRegex());

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, mapper);

    // Assert
    Vector<Object> filterReaders = mapper.getFilterReaders();
    assertEquals(1, filterReaders.size());
    Object getResult = filterReaders.get(0);
    assertTrue(getResult instanceof ContainsRegex);
    assertTrue(rmic.getCompileList().isEmpty());
    assertSame(project, ((ContainsRegex) getResult).getProject());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link FilterMapper} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenProject_whenFilterMapperProjectIsProject() {
    // Arrange
    Rmic rmic = new Rmic();

    FilterMapper mapper = new FilterMapper();
    mapper.setProject(new Project());
    mapper.addExpandProperties(new ExpandProperties());

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, mapper);

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) Idl is {@code false}.</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmicIdlIsFalse_whenArrayOfStringWithFiles() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setIdl(false);
    rmic.setIiop(false);
    rmic.setIiopopts(null);
    rmic.setDestdir(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) Iiop is {@code true}.</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmicIiopIsTrue_whenArrayOfStringWithFiles() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setIiop(true);

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmicProjectIsProject_whenArrayOfStringWithFiles() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setProject(new Project());

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .} and empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenArrayOfStringWithDotAndEmptyString() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{".", ""}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .} and {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenArrayOfStringWithDotAndNullFile() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{".", "NULL_FILE"}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .} and {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenArrayOfStringWithDotAndNullFile2() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{".", "NULL_FILE", "."}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When array of {@link String} with {@code ..}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenArrayOfStringWithDotDot_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{".."}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenArrayOfStringWithDot_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"."}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When array of {@link String} with empty string.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenArrayOfStringWithEmptyString_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{""}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When array of {@link String} with empty string.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenArrayOfStringWithEmptyString_thenRmicCompileListEmpty2() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{""}, new FilterMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenArrayOfStringWithFiles_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When {@link ChainedMapper} (default constructor).</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenChainedMapper_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new ChainedMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenEmptyArrayOfString_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{}, new CutDirsMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When {@link FilterMapper} (default constructor).</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenFilterMapper_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new FilterMapper());

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_givenRmic_whenNull_thenRmicCompileListEmpty() {
    // Arrange
    Rmic rmic = new Rmic();

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, null);

    // Assert that nothing has changed
    assertTrue(rmic.getCompileList().isEmpty());
  }

  /**
   * Test {@link Rmic#scanDir(File, String[], FileNameMapper)}.
   * <ul>
   *   <li>When array of {@link String} with {@code .class}.</li>
   *   <li>Then {@link Rmic} (default constructor) CompileList size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#scanDir(File, String[], FileNameMapper)}
   */
  @Test
  public void testScanDir_whenArrayOfStringWithClass_thenRmicCompileListSizeIsOne() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setIdl(true);

    // Act
    rmic.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{".class"}, new CutDirsMapper());

    // Assert
    Vector<String> compileList = rmic.getCompileList();
    assertEquals(1, compileList.size());
    assertEquals("", compileList.get(0));
  }

  /**
   * Test {@link Rmic#isValidRmiRemote(String)} with {@code classname}.
   * <p>
   * Method under test: {@link Rmic#isValidRmiRemote(String)}
   */
  @Test
  public void testIsValidRmiRemoteWithClassname() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry(Rmic.ERROR_UNABLE_TO_VERIFY_CLASS));

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertFalse(rmic.isValidRmiRemote("Classname"));
  }

  /**
   * Test {@link Rmic#isValidRmiRemote(String)} with {@code classname}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#isValidRmiRemote(String)}
   */
  @Test
  public void testIsValidRmiRemoteWithClassname_givenJavaLangObject_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(Rmic.ERROR_LOADING_CAUSED_EXCEPTION, typeClass);
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertFalse(rmic.isValidRmiRemote("Classname"));
  }

  /**
   * Test {@link Rmic#isValidRmiRemote(String)} with {@code classname}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#isValidRmiRemote(String)}
   */
  @Test
  public void testIsValidRmiRemoteWithClassname_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertFalse(rmic.isValidRmiRemote("Classname"));
  }

  /**
   * Test {@link Rmic#isValidRmiRemote(String)} with {@code classname}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#isValidRmiRemote(String)}
   */
  @Test
  public void testIsValidRmiRemoteWithClassname_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertFalse(rmic.isValidRmiRemote("Classname"));
  }

  /**
   * Test {@link Rmic#isValidRmiRemote(String)} with {@code classname}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link Recorder} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#isValidRmiRemote(String)}
   */
  @Test
  public void testIsValidRmiRemoteWithClassname_givenProjectAddBuildListenerRecorder() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new Recorder());

    Rmic rmic = new Rmic();
    rmic.setProject(project);

    // Act and Assert
    assertFalse(rmic.isValidRmiRemote("Classname"));
  }

  /**
   * Test {@link Rmic#isValidRmiRemote(String)} with {@code classname}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#isValidRmiRemote(String)}
   */
  @Test
  public void testIsValidRmiRemoteWithClassname_givenRmicProjectIsProject_thenReturnFalse() {
    // Arrange
    Rmic rmic = new Rmic();
    rmic.setProject(new Project());

    // Act and Assert
    assertFalse(rmic.isValidRmiRemote("Classname"));
  }

  /**
   * Test {@link Rmic#isValidRmiRemote(String)} with {@code classname}.
   * <ul>
   *   <li>Given {@link Rmic} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#isValidRmiRemote(String)}
   */
  @Test
  public void testIsValidRmiRemoteWithClassname_givenRmic_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Rmic()).isValidRmiRemote("Classname"));
  }

  /**
   * Test {@link Rmic#getRemoteInterface(Class)}.
   * <ul>
   *   <li>When {@code Object}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rmic#getRemoteInterface(Class)}
   */
  @Test
  public void testGetRemoteInterface_whenJavaLangObject_thenReturnNull() {
    // Arrange
    Rmic rmic = new Rmic();
    Class<Object> testClass = Object.class;

    // Act and Assert
    assertNull(rmic.getRemoteInterface(testClass));
  }
}
