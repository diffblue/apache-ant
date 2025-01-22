package org.apache.tools.ant.taskdefs;

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
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.Javac.ImplementationSpecificArgument;
import org.apache.tools.ant.taskdefs.compilers.CompilerAdapter;
import org.apache.tools.ant.taskdefs.compilers.Gcj;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class JavacDiffblueTest {
  /**
   * Test ImplementationSpecificArgument {@link ImplementationSpecificArgument#ImplementationSpecificArgument(Javac)}.
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#ImplementationSpecificArgument(Javac)}
   */
  @Test
  public void testImplementationSpecificArgumentNewImplementationSpecificArgument() {
    // Arrange and Act
    ImplementationSpecificArgument actualImplementationSpecificArgument = (new Javac()).new ImplementationSpecificArgument();

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
   * Test new {@link Javac} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Javac}
   */
  @Test
  public void testNewJavac() {
    // Arrange and Act
    Javac actualJavac = new Javac();

    // Assert
    assertNull(actualJavac.getDestdir());
    assertNull(actualJavac.getNativeHeaderDir());
    assertNull(actualJavac.getTempdir());
    assertNull(actualJavac.getDescription());
    assertNull(actualJavac.getTaskName());
    assertNull(actualJavac.getTaskType());
    assertNull(actualJavac.getDebugLevel());
    assertNull(actualJavac.getEncoding());
    assertNull(actualJavac.getExecutable());
    assertNull(actualJavac.getMemoryInitialSize());
    assertNull(actualJavac.getMemoryMaximumSize());
    assertNull(actualJavac.getRelease());
    assertNull(actualJavac.getProject());
    assertNull(actualJavac.getOwningTarget());
    assertNull(actualJavac.getBootclasspath());
    assertNull(actualJavac.getClasspath());
    assertNull(actualJavac.getExtdirs());
    assertNull(actualJavac.getModulepath());
    assertNull(actualJavac.getModulesourcepath());
    assertNull(actualJavac.getSourcepath());
    assertNull(actualJavac.getSrcdir());
    assertNull(actualJavac.getUpgrademodulepath());
    assertEquals(0, actualJavac.getFileList().length);
    assertFalse(actualJavac.getDebug());
    assertFalse(actualJavac.getDepend());
    assertFalse(actualJavac.getDeprecation());
    assertFalse(actualJavac.getIncludejavaruntime());
    assertFalse(actualJavac.getListfiles());
    assertFalse(actualJavac.getNowarn());
    assertFalse(actualJavac.getOptimize());
    assertFalse(actualJavac.getVerbose());
    assertFalse(actualJavac.hasSelectors());
    assertTrue(actualJavac.getFailonerror());
    assertTrue(actualJavac.getIncludeantruntime());
    assertTrue(actualJavac.getTaskSuccess());
    assertTrue(actualJavac.isIncludeDestClasses());
    String expectedSystemJavac = Paths.get(System.getProperty("java.home"), "bin", "javac").toString();
    assertEquals(expectedSystemJavac, actualJavac.getSystemJavac());
  }

  /**
   * Test {@link Javac#getSource()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getSource()}
   */
  @Test
  public void testGetSource_givenJavaLangObject_thenReturnNull() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getSource());
  }

  /**
   * Test {@link Javac#getSource()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getSource()}
   */
  @Test
  public void testGetSource_givenJavacProjectIsProject_thenReturnNull() {
    // Arrange
    Javac javac = new Javac();
    javac.setProject(new Project());

    // Act and Assert
    assertNull(javac.getSource());
  }

  /**
   * Test {@link Javac#getSource()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getSource()}
   */
  @Test
  public void testGetSource_givenProjectAddBuildListenerAntClassLoader_thenReturnNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getSource());
  }

  /**
   * Test {@link Javac#getSource()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getSource()}
   */
  @Test
  public void testGetSource_givenProjectAddTargetAntPropertyHelperAndTarget_thenReturnNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getSource());
  }

  /**
   * Test {@link Javac#getSource()}.
   * <ul>
   *   <li>Then return {@code ant.build.javac.source}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getSource()}
   */
  @Test
  public void testGetSource_thenReturnAntBuildJavacSource() {
    // Arrange
    Javac javac = new Javac();
    javac.setSource("ant.build.javac.source");

    // Act and Assert
    assertEquals("ant.build.javac.source", javac.getSource());
  }

  /**
   * Test {@link Javac#createSrc()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Srcdir Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createSrc()}
   */
  @Test
  public void testCreateSrc_givenJavac_thenJavacSrcdirDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualCreateSrcResult = javac.createSrc();

    // Assert
    Path srcdir = javac.getSrcdir();
    assertNull(srcdir.getDescription());
    assertNull(actualCreateSrcResult.getProject());
    assertNull(srcdir.getProject());
    assertNull(srcdir.getRefid());
    assertEquals(0, srcdir.size());
    assertFalse(srcdir.isReference());
    assertTrue(srcdir.isEmpty());
  }

  /**
   * Test {@link Javac#createSrc()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Srcdir is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createSrc()}
   */
  @Test
  public void testCreateSrc_thenJavacSrcdirIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setSrcdir(Path.systemBootClasspath);

    // Act and Assert
    Path expectedSrcdir = javac.createSrc().systemBootClasspath;
    assertSame(expectedSrcdir, javac.getSrcdir());
  }

  /**
   * Test {@link Javac#recreateSrc()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#recreateSrc()}
   */
  @Test
  public void testRecreateSrc_givenJavacProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Javac javac = new Javac();
    Project project = new Project();
    javac.setProject(project);

    // Act and Assert
    assertSame(project, javac.recreateSrc().getProject());
  }

  /**
   * Test {@link Javac#recreateSrc()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#recreateSrc()}
   */
  @Test
  public void testRecreateSrc_givenJavac_thenReturnLocationFileNameIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualRecreateSrcResult = javac.recreateSrc();

    // Assert
    Location location = actualRecreateSrcResult.getLocation();
    assertNull(location.getFileName());
    Path srcdir = javac.getSrcdir();
    assertNull(srcdir.getDescription());
    assertNull(actualRecreateSrcResult.getDescription());
    assertNull(srcdir.getProject());
    assertNull(actualRecreateSrcResult.getProject());
    assertNull(srcdir.getRefid());
    assertNull(actualRecreateSrcResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, srcdir.size());
    assertEquals(0, actualRecreateSrcResult.size());
    assertFalse(srcdir.isReference());
    assertFalse(actualRecreateSrcResult.isReference());
    assertTrue(srcdir.isEmpty());
    assertTrue(actualRecreateSrcResult.isEmpty());
  }

  /**
   * Test {@link Javac#setSrcdir(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Javac} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setSrcdir(Path)}
   */
  @Test
  public void testSetSrcdir_whenNull_thenJavacRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Javac javac = new Javac();
    javac.setSrcdir(Path.systemBootClasspath);

    // Act
    javac.setSrcdir(null);

    // Assert that nothing has changed
    assertTrue(javac.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Javac#setSourcepath(Path)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setSourcepath(Path)}
   */
  @Test
  public void testSetSourcepath_givenNull_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    Javac javac = new Javac();
    Project project = new Project();
    javac.setSourcepath(new Path(project));
    Path sourcepath = Path.systemBootClasspath;
    sourcepath.setProject(null);

    // Act
    javac.setSourcepath(sourcepath);

    // Assert
    assertSame(project, sourcepath.getProject());
  }

  /**
   * Test {@link Javac#createSourcepath()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Sourcepath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createSourcepath()}
   */
  @Test
  public void testCreateSourcepath_givenJavac_thenJavacSourcepathDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualCreateSourcepathResult = javac.createSourcepath();

    // Assert
    Path sourcepath = javac.getSourcepath();
    assertNull(sourcepath.getDescription());
    assertNull(actualCreateSourcepathResult.getProject());
    assertNull(sourcepath.getProject());
    assertNull(sourcepath.getRefid());
    assertEquals(0, sourcepath.size());
    assertFalse(sourcepath.isReference());
    assertTrue(sourcepath.isEmpty());
  }

  /**
   * Test {@link Javac#createSourcepath()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Sourcepath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createSourcepath()}
   */
  @Test
  public void testCreateSourcepath_thenJavacSourcepathIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setSourcepath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedSourcepath = javac.createSourcepath().systemBootClasspath;
    assertSame(expectedSourcepath, javac.getSourcepath());
  }

  /**
   * Test {@link Javac#setSourcepathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Sourcepath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setSourcepathRef(Reference)}
   */
  @Test
  public void testSetSourcepathRef_givenJavac_thenJavacSourcepathProjectIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setSourcepathRef(new Reference("42"));

    // Assert
    Path sourcepath = javac.getSourcepath();
    assertNull(sourcepath.getDescription());
    assertNull(sourcepath.getProject());
    assertNull(sourcepath.getRefid());
    assertFalse(sourcepath.isReference());
  }

  /**
   * Test {@link Javac#setModulesourcepath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setModulesourcepath(Path)}
   */
  @Test
  public void testSetModulesourcepath_whenNull() {
    // Arrange
    Javac javac = new Javac();
    javac.setModulesourcepath(Path.systemBootClasspath);

    // Act
    javac.setModulesourcepath(null);

    // Assert that nothing has changed
    assertTrue(javac.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Javac#createModulesourcepath()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Modulesourcepath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createModulesourcepath()}
   */
  @Test
  public void testCreateModulesourcepath_givenJavac_thenJavacModulesourcepathDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualCreateModulesourcepathResult = javac.createModulesourcepath();

    // Assert
    Path modulesourcepath = javac.getModulesourcepath();
    assertNull(modulesourcepath.getDescription());
    assertNull(actualCreateModulesourcepathResult.getProject());
    assertNull(modulesourcepath.getProject());
    assertNull(modulesourcepath.getRefid());
    assertEquals(0, modulesourcepath.size());
    assertFalse(modulesourcepath.isReference());
    assertTrue(modulesourcepath.isEmpty());
  }

  /**
   * Test {@link Javac#createModulesourcepath()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Modulesourcepath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createModulesourcepath()}
   */
  @Test
  public void testCreateModulesourcepath_thenJavacModulesourcepathIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setModulesourcepath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedModulesourcepath = javac.createModulesourcepath().systemBootClasspath;
    assertSame(expectedModulesourcepath, javac.getModulesourcepath());
  }

  /**
   * Test {@link Javac#setModulesourcepathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Modulesourcepath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setModulesourcepathRef(Reference)}
   */
  @Test
  public void testSetModulesourcepathRef_givenJavac_thenJavacModulesourcepathProjectIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setModulesourcepathRef(new Reference("42"));

    // Assert
    Path modulesourcepath = javac.getModulesourcepath();
    assertNull(modulesourcepath.getDescription());
    assertNull(modulesourcepath.getProject());
    assertNull(modulesourcepath.getRefid());
    assertFalse(modulesourcepath.isReference());
  }

  /**
   * Test {@link Javac#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Javac} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenJavacRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Javac javac = new Javac();
    javac.setClasspath(Path.systemBootClasspath);

    // Act
    javac.setClasspath(null);

    // Assert that nothing has changed
    assertTrue(javac.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Javac#createClasspath()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJavac_thenJavacClasspathDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualCreateClasspathResult = javac.createClasspath();

    // Assert
    Path classpath = javac.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link Javac#createClasspath()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenJavacClasspathIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = javac.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, javac.getClasspath());
  }

  /**
   * Test {@link Javac#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenJavac_thenJavacClasspathProjectIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = javac.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link Javac#setModulepath(Path)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setModulepath(Path)}
   */
  @Test
  public void testSetModulepath_givenNull_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    Javac javac = new Javac();
    Project project = new Project();
    javac.setModulepath(new Path(project));
    Path mp = Path.systemBootClasspath;
    mp.setProject(null);

    // Act
    javac.setModulepath(mp);

    // Assert
    assertSame(project, mp.getProject());
  }

  /**
   * Test {@link Javac#createModulepath()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Modulepath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createModulepath()}
   */
  @Test
  public void testCreateModulepath_givenJavac_thenJavacModulepathDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualCreateModulepathResult = javac.createModulepath();

    // Assert
    Path modulepath = javac.getModulepath();
    assertNull(modulepath.getDescription());
    assertNull(actualCreateModulepathResult.getProject());
    assertNull(modulepath.getProject());
    assertNull(modulepath.getRefid());
    assertEquals(0, modulepath.size());
    assertFalse(modulepath.isReference());
    assertTrue(modulepath.isEmpty());
  }

  /**
   * Test {@link Javac#createModulepath()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Modulepath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createModulepath()}
   */
  @Test
  public void testCreateModulepath_thenJavacModulepathIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setModulepath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedModulepath = javac.createModulepath().systemBootClasspath;
    assertSame(expectedModulepath, javac.getModulepath());
  }

  /**
   * Test {@link Javac#setModulepathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Modulepath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setModulepathRef(Reference)}
   */
  @Test
  public void testSetModulepathRef_givenJavac_thenJavacModulepathProjectIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setModulepathRef(new Reference("42"));

    // Assert
    Path modulepath = javac.getModulepath();
    assertNull(modulepath.getDescription());
    assertNull(modulepath.getProject());
    assertNull(modulepath.getRefid());
    assertFalse(modulepath.isReference());
  }

  /**
   * Test {@link Javac#setUpgrademodulepath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setUpgrademodulepath(Path)}
   */
  @Test
  public void testSetUpgrademodulepath_whenNull() {
    // Arrange
    Javac javac = new Javac();
    javac.setUpgrademodulepath(Path.systemBootClasspath);

    // Act
    javac.setUpgrademodulepath(null);

    // Assert that nothing has changed
    assertTrue(javac.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Javac#createUpgrademodulepath()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Upgrademodulepath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createUpgrademodulepath()}
   */
  @Test
  public void testCreateUpgrademodulepath_thenJavacUpgrademodulepathDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualCreateUpgrademodulepathResult = javac.createUpgrademodulepath();

    // Assert
    Path upgrademodulepath = javac.getUpgrademodulepath();
    assertNull(upgrademodulepath.getDescription());
    assertNull(actualCreateUpgrademodulepathResult.getProject());
    assertNull(upgrademodulepath.getProject());
    assertNull(upgrademodulepath.getRefid());
    assertEquals(0, upgrademodulepath.size());
    assertFalse(upgrademodulepath.isReference());
    assertTrue(upgrademodulepath.isEmpty());
  }

  /**
   * Test {@link Javac#createUpgrademodulepath()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Upgrademodulepath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createUpgrademodulepath()}
   */
  @Test
  public void testCreateUpgrademodulepath_thenJavacUpgrademodulepathIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setUpgrademodulepath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedUpgrademodulepath = javac.createUpgrademodulepath().systemBootClasspath;
    assertSame(expectedUpgrademodulepath, javac.getUpgrademodulepath());
  }

  /**
   * Test {@link Javac#setUpgrademodulepathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Upgrademodulepath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setUpgrademodulepathRef(Reference)}
   */
  @Test
  public void testSetUpgrademodulepathRef_givenJavac_thenJavacUpgrademodulepathProjectIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setUpgrademodulepathRef(new Reference("42"));

    // Assert
    Path upgrademodulepath = javac.getUpgrademodulepath();
    assertNull(upgrademodulepath.getDescription());
    assertNull(upgrademodulepath.getProject());
    assertNull(upgrademodulepath.getRefid());
    assertFalse(upgrademodulepath.isReference());
  }

  /**
   * Test {@link Javac#setBootclasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setBootclasspath(Path)}
   */
  @Test
  public void testSetBootclasspath_whenNull() {
    // Arrange
    Javac javac = new Javac();
    javac.setBootclasspath(Path.systemBootClasspath);

    // Act
    javac.setBootclasspath(null);

    // Assert that nothing has changed
    assertTrue(javac.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Javac#setCreateMissingPackageInfoClass(boolean)}
   *   <li>{@link Javac#setDebug(boolean)}
   *   <li>{@link Javac#setDebugLevel(String)}
   *   <li>{@link Javac#setDepend(boolean)}
   *   <li>{@link Javac#setDeprecation(boolean)}
   *   <li>{@link Javac#setDestdir(File)}
   *   <li>{@link Javac#setEncoding(String)}
   *   <li>{@link Javac#setErrorProperty(String)}
   *   <li>{@link Javac#setExecutable(String)}
   *   <li>{@link Javac#setFailonerror(boolean)}
   *   <li>{@link Javac#setFork(boolean)}
   *   <li>{@link Javac#setIncludeDestClasses(boolean)}
   *   <li>{@link Javac#setIncludejavaruntime(boolean)}
   *   <li>{@link Javac#setListfiles(boolean)}
   *   <li>{@link Javac#setMemoryInitialSize(String)}
   *   <li>{@link Javac#setMemoryMaximumSize(String)}
   *   <li>{@link Javac#setNativeHeaderDir(File)}
   *   <li>{@link Javac#setNowarn(boolean)}
   *   <li>{@link Javac#setOptimize(boolean)}
   *   <li>{@link Javac#setRelease(String)}
   *   <li>{@link Javac#setSource(String)}
   *   <li>{@link Javac#setTarget(String)}
   *   <li>{@link Javac#setTempdir(File)}
   *   <li>{@link Javac#setUpdatedProperty(String)}
   *   <li>{@link Javac#setVerbose(boolean)}
   *   <li>{@link Javac#getBootclasspath()}
   *   <li>{@link Javac#getClasspath()}
   *   <li>{@link Javac#getDebug()}
   *   <li>{@link Javac#getDebugLevel()}
   *   <li>{@link Javac#getDepend()}
   *   <li>{@link Javac#getDeprecation()}
   *   <li>{@link Javac#getDestdir()}
   *   <li>{@link Javac#getEncoding()}
   *   <li>{@link Javac#getExecutable()}
   *   <li>{@link Javac#getExtdirs()}
   *   <li>{@link Javac#getFailonerror()}
   *   <li>{@link Javac#getFileList()}
   *   <li>{@link Javac#getIncludejavaruntime()}
   *   <li>{@link Javac#getListfiles()}
   *   <li>{@link Javac#getMemoryInitialSize()}
   *   <li>{@link Javac#getMemoryMaximumSize()}
   *   <li>{@link Javac#getModulepath()}
   *   <li>{@link Javac#getModulesourcepath()}
   *   <li>{@link Javac#getNativeHeaderDir()}
   *   <li>{@link Javac#getNowarn()}
   *   <li>{@link Javac#getOptimize()}
   *   <li>{@link Javac#getRelease()}
   *   <li>{@link Javac#getSourcepath()}
   *   <li>{@link Javac#getSrcdir()}
   *   <li>{@link Javac#getTaskSuccess()}
   *   <li>{@link Javac#getTempdir()}
   *   <li>{@link Javac#getUpgrademodulepath()}
   *   <li>{@link Javac#getVerbose()}
   *   <li>{@link Javac#isIncludeDestClasses()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setCreateMissingPackageInfoClass(true);
    javac.setDebug(true);
    javac.setDebugLevel("foo");
    javac.setDepend(true);
    javac.setDeprecation(true);
    javac.setDestdir(Copy.NULL_FILE_PLACEHOLDER);
    javac.setEncoding(Manifest.JAR_ENCODING);
    javac.setErrorProperty("An error occurred");
    javac.setExecutable("Fork Exec");
    javac.setFailonerror(true);
    javac.setFork(true);
    javac.setIncludeDestClasses(true);
    javac.setIncludejavaruntime(true);
    javac.setListfiles(true);
    javac.setMemoryInitialSize("Memory Initial Size");
    javac.setMemoryMaximumSize("Memory Maximum Size");
    javac.setNativeHeaderDir(Copy.NULL_FILE_PLACEHOLDER);
    javac.setNowarn(true);
    javac.setOptimize(true);
    javac.setRelease("1.0.2");
    javac.setSource("foo");
    javac.setTarget("Target");
    File tmpDir = Copy.NULL_FILE_PLACEHOLDER;
    javac.setTempdir(tmpDir);
    javac.setUpdatedProperty("2020-03-01");
    javac.setVerbose(true);
    Path actualBootclasspath = javac.getBootclasspath();
    Path actualClasspath = javac.getClasspath();
    boolean actualDebug = javac.getDebug();
    String actualDebugLevel = javac.getDebugLevel();
    boolean actualDepend = javac.getDepend();
    boolean actualDeprecation = javac.getDeprecation();
    File actualDestdir = javac.getDestdir();
    String actualEncoding = javac.getEncoding();
    String actualExecutable = javac.getExecutable();
    Path actualExtdirs = javac.getExtdirs();
    boolean actualFailonerror = javac.getFailonerror();
    File[] actualFileList = javac.getFileList();
    boolean actualIncludejavaruntime = javac.getIncludejavaruntime();
    boolean actualListfiles = javac.getListfiles();
    String actualMemoryInitialSize = javac.getMemoryInitialSize();
    String actualMemoryMaximumSize = javac.getMemoryMaximumSize();
    Path actualModulepath = javac.getModulepath();
    Path actualModulesourcepath = javac.getModulesourcepath();
    File actualNativeHeaderDir = javac.getNativeHeaderDir();
    boolean actualNowarn = javac.getNowarn();
    boolean actualOptimize = javac.getOptimize();
    String actualRelease = javac.getRelease();
    Path actualSourcepath = javac.getSourcepath();
    Path actualSrcdir = javac.getSrcdir();
    boolean actualTaskSuccess = javac.getTaskSuccess();
    File actualTempdir = javac.getTempdir();
    Path actualUpgrademodulepath = javac.getUpgrademodulepath();
    boolean actualVerbose = javac.getVerbose();

    // Assert
    assertEquals("1.0.2", actualRelease);
    assertEquals("Fork Exec", actualExecutable);
    assertEquals("Memory Initial Size", actualMemoryInitialSize);
    assertEquals("Memory Maximum Size", actualMemoryMaximumSize);
    assertEquals("foo", actualDebugLevel);
    assertNull(actualBootclasspath);
    assertNull(actualClasspath);
    assertNull(actualExtdirs);
    assertNull(actualModulepath);
    assertNull(actualModulesourcepath);
    assertNull(actualSourcepath);
    assertNull(actualSrcdir);
    assertNull(actualUpgrademodulepath);
    assertEquals(0, actualFileList.length);
    assertTrue(actualDebug);
    assertTrue(actualDepend);
    assertTrue(actualDeprecation);
    assertTrue(actualFailonerror);
    assertTrue(actualIncludejavaruntime);
    assertTrue(actualListfiles);
    assertTrue(actualNowarn);
    assertTrue(actualOptimize);
    assertTrue(actualTaskSuccess);
    assertTrue(actualVerbose);
    assertTrue(javac.isIncludeDestClasses());
    assertEquals(Manifest.JAR_ENCODING, actualEncoding);
    assertSame(tmpDir, actualDestdir);
    assertSame(tmpDir, actualNativeHeaderDir);
    assertSame(tmpDir, actualTempdir);
  }

  /**
   * Test {@link Javac#createBootclasspath()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Bootclasspath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_givenJavac_thenJavacBootclasspathDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualCreateBootclasspathResult = javac.createBootclasspath();

    // Assert
    Path bootclasspath = javac.getBootclasspath();
    assertNull(bootclasspath.getDescription());
    assertNull(actualCreateBootclasspathResult.getProject());
    assertNull(bootclasspath.getProject());
    assertNull(bootclasspath.getRefid());
    assertEquals(0, bootclasspath.size());
    assertFalse(bootclasspath.isReference());
    assertTrue(bootclasspath.isEmpty());
  }

  /**
   * Test {@link Javac#createBootclasspath()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Bootclasspath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_thenJavacBootclasspathIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setBootclasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedBootclasspath = javac.createBootclasspath().systemBootClasspath;
    assertSame(expectedBootclasspath, javac.getBootclasspath());
  }

  /**
   * Test {@link Javac#setBootClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Bootclasspath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setBootClasspathRef(Reference)}
   */
  @Test
  public void testSetBootClasspathRef_givenJavacBootclasspathIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setBootclasspath(Path.systemBootClasspath);

    // Act
    javac.setBootClasspathRef(new Reference("42"));

    // Assert that nothing has changed
    assertFalse(javac.getBootclasspath().isReference());
  }

  /**
   * Test {@link Javac#setBootClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Bootclasspath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setBootClasspathRef(Reference)}
   */
  @Test
  public void testSetBootClasspathRef_givenJavac_thenJavacBootclasspathDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setBootClasspathRef(new Reference("42"));

    // Assert
    Path bootclasspath = javac.getBootclasspath();
    assertNull(bootclasspath.getDescription());
    assertNull(bootclasspath.getProject());
    assertNull(bootclasspath.getRefid());
    assertFalse(bootclasspath.isReference());
  }

  /**
   * Test {@link Javac#setExtdirs(Path)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setExtdirs(Path)}
   */
  @Test
  public void testSetExtdirs_givenNull_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    Javac javac = new Javac();
    Project project = new Project();
    javac.setExtdirs(new Path(project));
    Path extdirs = Path.systemBootClasspath;
    extdirs.setProject(null);

    // Act
    javac.setExtdirs(extdirs);

    // Assert
    assertSame(project, extdirs.getProject());
  }

  /**
   * Test {@link Javac#createExtdirs()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then {@link Javac} (default constructor) Extdirs Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createExtdirs()}
   */
  @Test
  public void testCreateExtdirs_givenJavac_thenJavacExtdirsDescriptionIsNull() {
    // Arrange
    Javac javac = new Javac();

    // Act
    Path actualCreateExtdirsResult = javac.createExtdirs();

    // Assert
    Path extdirs = javac.getExtdirs();
    assertNull(extdirs.getDescription());
    assertNull(actualCreateExtdirsResult.getProject());
    assertNull(extdirs.getProject());
    assertNull(extdirs.getRefid());
    assertEquals(0, extdirs.size());
    assertFalse(extdirs.isReference());
    assertTrue(extdirs.isEmpty());
  }

  /**
   * Test {@link Javac#createExtdirs()}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Extdirs is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#createExtdirs()}
   */
  @Test
  public void testCreateExtdirs_thenJavacExtdirsIsSystemBootClasspath() {
    // Arrange
    Javac javac = new Javac();
    javac.setExtdirs(Path.systemBootClasspath);

    // Act and Assert
    Path expectedExtdirs = javac.createExtdirs().systemBootClasspath;
    assertSame(expectedExtdirs, javac.getExtdirs());
  }

  /**
   * Test {@link Javac#setProceed(boolean)}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then {@link Javac} (default constructor) Failonerror.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setProceed(boolean)}
   */
  @Test
  public void testSetProceed_whenFalse_thenJavacFailonerror() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setProceed(false);

    // Assert that nothing has changed
    assertTrue(javac.getFailonerror());
  }

  /**
   * Test {@link Javac#setProceed(boolean)}.
   * <ul>
   *   <li>When {@code true}.</li>
   *   <li>Then not {@link Javac} (default constructor) Failonerror.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#setProceed(boolean)}
   */
  @Test
  public void testSetProceed_whenTrue_thenNotJavacFailonerror() {
    // Arrange
    Javac javac = new Javac();

    // Act
    javac.setProceed(true);

    // Assert
    assertFalse(javac.getFailonerror());
  }

  /**
   * Test {@link Javac#getTarget()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getTarget()}
   */
  @Test
  public void testGetTarget_givenJavaLangObject_thenReturnNull() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getTarget());
  }

  /**
   * Test {@link Javac#getTarget()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getTarget()}
   */
  @Test
  public void testGetTarget_givenJavacProjectIsProject_thenReturnNull() {
    // Arrange
    Javac javac = new Javac();
    javac.setProject(new Project());

    // Act and Assert
    assertNull(javac.getTarget());
  }

  /**
   * Test {@link Javac#getTarget()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getTarget()}
   */
  @Test
  public void testGetTarget_givenProjectAddBuildListenerAntClassLoader_thenReturnNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getTarget());
  }

  /**
   * Test {@link Javac#getTarget()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getTarget()}
   */
  @Test
  public void testGetTarget_givenProjectAddTargetAntPropertyHelperAndTarget_thenReturnNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getTarget());
  }

  /**
   * Test {@link Javac#getTarget()}.
   * <ul>
   *   <li>Then return {@code ant.build.javac.target}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getTarget()}
   */
  @Test
  public void testGetTarget_thenReturnAntBuildJavacTarget() {
    // Arrange
    Javac javac = new Javac();
    javac.setTarget("ant.build.javac.target");

    // Act and Assert
    assertEquals("ant.build.javac.target", javac.getTarget());
  }

  /**
   * Test {@link Javac#getIncludeantruntime()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Includeantruntime is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getIncludeantruntime()}
   */
  @Test
  public void testGetIncludeantruntime_givenJavacIncludeantruntimeIsFalse_thenReturnFalse() {
    // Arrange
    Javac javac = new Javac();
    javac.setIncludeantruntime(false);

    // Act and Assert
    assertFalse(javac.getIncludeantruntime());
  }

  /**
   * Test {@link Javac#getIncludeantruntime()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Includeantruntime is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getIncludeantruntime()}
   */
  @Test
  public void testGetIncludeantruntime_givenJavacIncludeantruntimeIsTrue_thenReturnTrue() {
    // Arrange
    Javac javac = new Javac();
    javac.setIncludeantruntime(true);

    // Act and Assert
    assertTrue(javac.getIncludeantruntime());
  }

  /**
   * Test {@link Javac#getIncludeantruntime()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getIncludeantruntime()}
   */
  @Test
  public void testGetIncludeantruntime_givenJavac_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Javac()).getIncludeantruntime());
  }

  /**
   * Test {@link Javac#isForkedJavac()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#isForkedJavac()}
   */
  @Test
  public void testIsForkedJavac_givenJavaLangObject_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertFalse(javac.isForkedJavac());
  }

  /**
   * Test {@link Javac#isForkedJavac()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Fork is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#isForkedJavac()}
   */
  @Test
  public void testIsForkedJavac_givenJavacForkIsTrue_thenReturnTrue() {
    // Arrange
    Javac javac = new Javac();
    javac.setFork(true);

    // Act and Assert
    assertTrue(javac.isForkedJavac());
  }

  /**
   * Test {@link Javac#isForkedJavac()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#isForkedJavac()}
   */
  @Test
  public void testIsForkedJavac_givenJavacProjectIsProject_thenReturnFalse() {
    // Arrange
    Javac javac = new Javac();
    javac.setProject(new Project());

    // Act and Assert
    assertFalse(javac.isForkedJavac());
  }

  /**
   * Test {@link Javac#isForkedJavac()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#isForkedJavac()}
   */
  @Test
  public void testIsForkedJavac_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertFalse(javac.isForkedJavac());
  }

  /**
   * Test {@link Javac#isForkedJavac()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#isForkedJavac()}
   */
  @Test
  public void testIsForkedJavac_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertFalse(javac.isForkedJavac());
  }

  /**
   * Test {@link Javac#getJavacExecutable()}.
   * <p>
   * Method under test: {@link Javac#getJavacExecutable()}
   */
  @Test
  public void testGetJavacExecutable() {
    // Arrange
    Javac javac = new Javac();
    javac.setFork(true);

    // Act
    String actualJavacExecutable = javac.getJavacExecutable();

    // Assert
    assertEquals(Paths.get(System.getProperty("java.home"), "bin", "javac").toString(), actualJavacExecutable);
  }

  /**
   * Test {@link Javac#getJavacExecutable()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getJavacExecutable()}
   */
  @Test
  public void testGetJavacExecutable_givenJavaLangObject_thenReturnNull() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getJavacExecutable());
  }

  /**
   * Test {@link Javac#getJavacExecutable()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getJavacExecutable()}
   */
  @Test
  public void testGetJavacExecutable_givenJavacProjectIsProject_thenReturnNull() {
    // Arrange
    Javac javac = new Javac();
    javac.setProject(new Project());

    // Act and Assert
    assertNull(javac.getJavacExecutable());
  }

  /**
   * Test {@link Javac#getJavacExecutable()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getJavacExecutable()}
   */
  @Test
  public void testGetJavacExecutable_givenProjectAddBuildListenerAntClassLoader_thenReturnNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getJavacExecutable());
  }

  /**
   * Test {@link Javac#getJavacExecutable()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getJavacExecutable()}
   */
  @Test
  public void testGetJavacExecutable_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertNull(javac.getJavacExecutable());
  }

  /**
   * Test {@link Javac#createCompilerArg()}.
   * <p>
   * Method under test: {@link Javac#createCompilerArg()}
   */
  @Test
  public void testCreateCompilerArg() {
    // Arrange and Act
    ImplementationSpecificArgument actualCreateCompilerArgResult = (new Javac()).createCompilerArg();

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
   * Test {@link Javac#getCurrentCompilerArgs()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCurrentCompilerArgs()}
   */
  @Test
  public void testGetCurrentCompilerArgs_givenJavaLangObject_thenReturnArrayLengthIsZero() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals(0, javac.getCurrentCompilerArgs().length);
  }

  /**
   * Test {@link Javac#getCurrentCompilerArgs()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCurrentCompilerArgs()}
   */
  @Test
  public void testGetCurrentCompilerArgs_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals(0, javac.getCurrentCompilerArgs().length);
  }

  /**
   * Test {@link Javac#getCurrentCompilerArgs()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCurrentCompilerArgs()}
   */
  @Test
  public void testGetCurrentCompilerArgs_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals(0, javac.getCurrentCompilerArgs().length);
  }

  /**
   * Test {@link Javac#getCurrentCompilerArgs()}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCurrentCompilerArgs()}
   */
  @Test
  public void testGetCurrentCompilerArgs_thenReturnArrayLengthIsZero() {
    // Arrange
    Javac javac = new Javac();
    javac.setProject(new Project());

    // Act and Assert
    assertEquals(0, javac.getCurrentCompilerArgs().length);
  }

  /**
   * Test {@link Javac#createCompilerClasspath()}.
   * <p>
   * Method under test: {@link Javac#createCompilerClasspath()}
   */
  @Test
  public void testCreateCompilerClasspath() {
    // Arrange and Act
    Path actualCreateCompilerClasspathResult = (new Javac()).createCompilerClasspath();

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
   * Test {@link Javac#add(CompilerAdapter)} with {@code adapter}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) add {@link Gcj} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#add(CompilerAdapter)}
   */
  @Test
  public void testAddWithAdapter_givenJavacAddGcj_thenThrowBuildException() {
    // Arrange
    Javac javac = new Javac();
    javac.add(new Gcj());

    // Act and Assert
    assertThrows(BuildException.class, () -> javac.add(new Gcj()));
  }

  /**
   * Test {@link Javac#execute()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Srcdir is {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#execute()}
   */
  @Test
  public void testExecute_givenJavacSrcdirIsPathWithProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Javac javac = new Javac();
    javac.setSrcdir(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> javac.execute());
  }

  /**
   * Test {@link Javac#execute()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#execute()}
   */
  @Test
  public void testExecute_givenJavac_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Javac()).execute());
  }

  /**
   * Test {@link Javac#isJdkCompiler(String)}.
   * <ul>
   *   <li>When {@code Compiler Impl}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#isJdkCompiler(String)}
   */
  @Test
  public void testIsJdkCompiler_whenCompilerImpl() {
    // Arrange, Act and Assert
    assertFalse((new Javac()).isJdkCompiler("Compiler Impl"));
  }

  /**
   * Test {@link Javac#isJdkCompiler(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#isJdkCompiler(String)}
   */
  @Test
  public void testIsJdkCompiler_whenNull() {
    // Arrange, Act and Assert
    assertFalse((new Javac()).isJdkCompiler(null));
  }

  /**
   * Test {@link Javac#getSystemJavac()}.
   * <p>
   * Method under test: {@link Javac#getSystemJavac()}
   */
  @Test
  public void testGetSystemJavac() {
    // Arrange and Act
    String actualSystemJavac = (new Javac()).getSystemJavac();

    // Assert
    assertEquals(Paths.get(System.getProperty("java.home"), "bin", "javac").toString(), actualSystemJavac);
  }

  /**
   * Test {@link Javac#getCompiler()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code javac10+}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCompiler()}
   */
  @Test
  public void testGetCompiler_givenJavaLangObject_thenReturnJavac10() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals("javac10+", javac.getCompiler());
  }

  /**
   * Test {@link Javac#getCompiler()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code javac10+}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCompiler()}
   */
  @Test
  public void testGetCompiler_givenJavacProjectIsProject_thenReturnJavac10() {
    // Arrange
    Javac javac = new Javac();
    javac.setProject(new Project());

    // Act and Assert
    assertEquals("javac10+", javac.getCompiler());
  }

  /**
   * Test {@link Javac#getCompiler()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code javac10+}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCompiler()}
   */
  @Test
  public void testGetCompiler_givenProjectAddBuildListenerAntClassLoader_thenReturnJavac10() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals("javac10+", javac.getCompiler());
  }

  /**
   * Test {@link Javac#getCompiler()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCompiler()}
   */
  @Test
  public void testGetCompiler_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals("javac10+", javac.getCompiler());
  }

  /**
   * Test {@link Javac#getCompilerVersion()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code javac10+}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCompilerVersion()}
   */
  @Test
  public void testGetCompilerVersion_givenJavaLangObject_thenReturnJavac10() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals("javac10+", javac.getCompilerVersion());
  }

  /**
   * Test {@link Javac#getCompilerVersion()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code javac10+}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCompilerVersion()}
   */
  @Test
  public void testGetCompilerVersion_givenJavacProjectIsProject_thenReturnJavac10() {
    // Arrange
    Javac javac = new Javac();
    javac.setProject(new Project());

    // Act and Assert
    assertEquals("javac10+", javac.getCompilerVersion());
  }

  /**
   * Test {@link Javac#getCompilerVersion()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCompilerVersion()}
   */
  @Test
  public void testGetCompilerVersion_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals("javac10+", javac.getCompilerVersion());
  }

  /**
   * Test {@link Javac#getCompilerVersion()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#getCompilerVersion()}
   */
  @Test
  public void testGetCompilerVersion_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Javac javac = new Javac();
    javac.setProject(project);

    // Act and Assert
    assertEquals("javac10+", javac.getCompilerVersion());
  }

  /**
   * Test {@link Javac#checkParameters()}.
   * <ul>
   *   <li>Given {@link Javac} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#checkParameters()}
   */
  @Test
  public void testCheckParameters_givenJavac_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Javac()).checkParameters());
  }

  /**
   * Test {@link Javac#checkParameters()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Javac#checkParameters()}
   */
  @Test
  public void testCheckParameters_thenThrowBuildException() throws BuildException {
    // Arrange
    Javac javac = new Javac();
    javac.setSrcdir(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> javac.checkParameters());
  }
}
