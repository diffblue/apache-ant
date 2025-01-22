package org.apache.tools.ant.taskdefs.modules;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.modules.Jmod.ResolutionWarningReason;
import org.apache.tools.ant.taskdefs.modules.Jmod.ResolutionWarningSpec;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.ModuleVersion;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class JmodDiffblueTest {
  /**
   * Test {@link Jmod#createClasspath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJmodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act and Assert
    assertSame(project, jmod.createClasspath().getProject());
  }

  /**
   * Test {@link Jmod#createClasspath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJmod_thenJmodClasspathDescriptionIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    Path actualCreateClasspathResult = jmod.createClasspath();

    // Assert
    Path classpath = jmod.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link Jmod#createClasspath()}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenJmodClasspathIsSystemBootClasspath() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = jmod.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, jmod.getClasspath());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Jmod#setDestFile(File)}
   *   <li>{@link Jmod#setHashModulesPattern(String)}
   *   <li>{@link Jmod#setMainClass(String)}
   *   <li>{@link Jmod#setPlatform(String)}
   *   <li>{@link Jmod#setResolveByDefault(boolean)}
   *   <li>{@link Jmod#setVersion(String)}
   *   <li>{@link Jmod#getClasspath()}
   *   <li>{@link Jmod#getCommandPath()}
   *   <li>{@link Jmod#getConfigPath()}
   *   <li>{@link Jmod#getDestFile()}
   *   <li>{@link Jmod#getHashModulesPattern()}
   *   <li>{@link Jmod#getHeaderPath()}
   *   <li>{@link Jmod#getLegalPath()}
   *   <li>{@link Jmod#getMainClass()}
   *   <li>{@link Jmod#getManPath()}
   *   <li>{@link Jmod#getModulePath()}
   *   <li>{@link Jmod#getNativeLibPath()}
   *   <li>{@link Jmod#getPlatform()}
   *   <li>{@link Jmod#getResolveByDefault()}
   *   <li>{@link Jmod#getVersion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Jmod jmod = new Jmod();
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    jmod.setDestFile(file);
    jmod.setHashModulesPattern("Pattern");
    jmod.setMainClass("Class Name");
    jmod.setPlatform("Platform");
    jmod.setResolveByDefault(true);
    jmod.setVersion("1.0.2");
    Path actualClasspath = jmod.getClasspath();
    Path actualCommandPath = jmod.getCommandPath();
    Path actualConfigPath = jmod.getConfigPath();
    File actualDestFile = jmod.getDestFile();
    String actualHashModulesPattern = jmod.getHashModulesPattern();
    Path actualHeaderPath = jmod.getHeaderPath();
    Path actualLegalPath = jmod.getLegalPath();
    String actualMainClass = jmod.getMainClass();
    Path actualManPath = jmod.getManPath();
    Path actualModulePath = jmod.getModulePath();
    Path actualNativeLibPath = jmod.getNativeLibPath();
    String actualPlatform = jmod.getPlatform();
    boolean actualResolveByDefault = jmod.getResolveByDefault();

    // Assert
    assertEquals("1.0.2", jmod.getVersion());
    assertEquals("Class Name", actualMainClass);
    assertEquals("Pattern", actualHashModulesPattern);
    assertEquals("Platform", actualPlatform);
    assertNull(actualClasspath);
    assertNull(actualCommandPath);
    assertNull(actualConfigPath);
    assertNull(actualHeaderPath);
    assertNull(actualLegalPath);
    assertNull(actualManPath);
    assertNull(actualModulePath);
    assertNull(actualNativeLibPath);
    assertTrue(actualResolveByDefault);
    assertSame(file, actualDestFile);
  }

  /**
   * Test ResolutionWarningReason {@link ResolutionWarningReason#getValues()}.
   * <p>
   * Method under test: {@link ResolutionWarningReason#getValues()}
   */
  @Test
  public void testResolutionWarningReasonGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{ResolutionWarningReason.DEPRECATED, ResolutionWarningReason.LEAVING,
        ResolutionWarningReason.INCUBATING}, (new ResolutionWarningReason()).getValues());
  }

  /**
   * Test ResolutionWarningReason new {@link ResolutionWarningReason} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ResolutionWarningReason}
   */
  @Test
  public void testResolutionWarningReasonNewResolutionWarningReason() {
    // Arrange and Act
    ResolutionWarningReason actualResolutionWarningReason = new ResolutionWarningReason();

    // Assert
    assertNull(actualResolutionWarningReason.getValue());
    assertEquals(-1, actualResolutionWarningReason.getIndex());
  }

  /**
   * Test ResolutionWarningReason {@link ResolutionWarningReason#toCommandLineOption()}.
   * <p>
   * Method under test: {@link ResolutionWarningReason#toCommandLineOption()}
   */
  @Test
  public void testResolutionWarningReasonToCommandLineOption() {
    // Arrange, Act and Assert
    assertNull((new ResolutionWarningReason()).toCommandLineOption());
  }

  /**
   * Test ResolutionWarningReason {@link ResolutionWarningReason#valueOf(String)}.
   * <ul>
   *   <li>When {@link ResolutionWarningReason#DEPRECATED}.</li>
   *   <li>Then return Index is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResolutionWarningReason#valueOf(String)}
   */
  @Test
  public void testResolutionWarningReasonValueOf_whenDeprecated_thenReturnIndexIsZero() {
    // Arrange and Act
    ResolutionWarningReason actualValueOfResult = ResolutionWarningReason.valueOf(ResolutionWarningReason.DEPRECATED);

    // Assert
    assertEquals(0, actualValueOfResult.getIndex());
    assertEquals(ResolutionWarningReason.DEPRECATED, actualValueOfResult.toCommandLineOption());
    assertEquals(ResolutionWarningReason.DEPRECATED, actualValueOfResult.getValue());
    assertArrayEquals(new String[]{ResolutionWarningReason.DEPRECATED, ResolutionWarningReason.LEAVING,
        ResolutionWarningReason.INCUBATING}, actualValueOfResult.getValues());
  }

  /**
   * Test ResolutionWarningSpec getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ResolutionWarningSpec#ResolutionWarningSpec(Jmod)}
   *   <li>{@link ResolutionWarningSpec#setReason(ResolutionWarningReason)}
   *   <li>{@link ResolutionWarningSpec#getReason()}
   * </ul>
   */
  @Test
  public void testResolutionWarningSpecGettersAndSetters() {
    // Arrange and Act
    ResolutionWarningSpec actualResolutionWarningSpec = (new Jmod()).new ResolutionWarningSpec();
    ResolutionWarningReason reason = new ResolutionWarningReason();
    actualResolutionWarningSpec.setReason(reason);

    // Assert
    assertSame(reason, actualResolutionWarningSpec.getReason());
  }

  /**
   * Test ResolutionWarningSpec {@link ResolutionWarningSpec#ResolutionWarningSpec(Jmod, String)}.
   * <ul>
   *   <li>Then return Reason Index is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResolutionWarningSpec#ResolutionWarningSpec(Jmod, String)}
   */
  @Test
  public void testResolutionWarningSpecNewResolutionWarningSpec_thenReturnReasonIndexIsZero() {
    // Arrange, Act and Assert
    ResolutionWarningReason reason = ((new Jmod()).new ResolutionWarningSpec(ResolutionWarningReason.DEPRECATED))
        .getReason();
    assertEquals(0, reason.getIndex());
    assertEquals(ResolutionWarningReason.DEPRECATED, reason.toCommandLineOption());
    assertEquals(ResolutionWarningReason.DEPRECATED, reason.getValue());
    assertArrayEquals(new String[]{ResolutionWarningReason.DEPRECATED, ResolutionWarningReason.LEAVING,
        ResolutionWarningReason.INCUBATING}, reason.getValues());
  }

  /**
   * Test ResolutionWarningSpec {@link ResolutionWarningSpec#validate()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResolutionWarningSpec#validate()}
   */
  @Test
  public void testResolutionWarningSpecValidate_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Jmod()).new ResolutionWarningSpec()).validate());
  }

  /**
   * Test {@link Jmod#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) Classpath is {@link Path#Path(Project)} with project is {@code null} {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenJmodClasspathIsPathWithProjectIsNullSystemBootClasspath() {
    // Arrange
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    Jmod jmod = new Jmod();
    jmod.setClasspath(path);
    Path path2 = new Path(null);

    // Act
    jmod.setClasspath(path2);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    Path expectedClasspath = path2.systemBootClasspath;
    assertSame(expectedClasspath, jmod.getClasspath());
  }

  /**
   * Test {@link Jmod#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Jmod} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenJmodRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setClasspath(Path.systemBootClasspath);

    // Act
    jmod.setClasspath(null);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Jmod#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) Classpath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenJmodProjectIsProject_thenJmodClasspathProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act
    jmod.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = jmod.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
    assertSame(project, classpath.getProject());
  }

  /**
   * Test {@link Jmod#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenJmod_thenJmodClasspathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    jmod.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = jmod.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link Jmod#createModulePath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createModulePath()}
   */
  @Test
  public void testCreateModulePath_givenJmodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act and Assert
    assertSame(project, jmod.createModulePath().getProject());
  }

  /**
   * Test {@link Jmod#createModulePath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) ModulePath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createModulePath()}
   */
  @Test
  public void testCreateModulePath_givenJmod_thenJmodModulePathDescriptionIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    Path actualCreateModulePathResult = jmod.createModulePath();

    // Assert
    Path modulePath = jmod.getModulePath();
    assertNull(modulePath.getDescription());
    assertNull(actualCreateModulePathResult.getProject());
    assertNull(modulePath.getProject());
    assertNull(modulePath.getRefid());
    assertEquals(0, modulePath.size());
    assertFalse(modulePath.isReference());
    assertTrue(modulePath.isEmpty());
  }

  /**
   * Test {@link Jmod#createModulePath()}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) ModulePath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createModulePath()}
   */
  @Test
  public void testCreateModulePath_thenJmodModulePathIsSystemBootClasspath() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setModulePath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedModulePath = jmod.createModulePath().systemBootClasspath;
    assertSame(expectedModulePath, jmod.getModulePath());
  }

  /**
   * Test {@link Jmod#setModulePath(Path)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) ModulePath is {@link Path#Path(Project)} with project is {@code null} {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setModulePath(Path)}
   */
  @Test
  public void testSetModulePath_thenJmodModulePathIsPathWithProjectIsNullSystemBootClasspath() {
    // Arrange
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    Jmod jmod = new Jmod();
    jmod.setModulePath(path);
    Path path2 = new Path(null);

    // Act
    jmod.setModulePath(path2);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    Path expectedModulePath = path2.systemBootClasspath;
    assertSame(expectedModulePath, jmod.getModulePath());
  }

  /**
   * Test {@link Jmod#setModulePath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Jmod} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setModulePath(Path)}
   */
  @Test
  public void testSetModulePath_whenNull_thenJmodRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setModulePath(Path.systemBootClasspath);

    // Act
    jmod.setModulePath(null);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Jmod#setModulePathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) ModulePath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setModulePathRef(Reference)}
   */
  @Test
  public void testSetModulePathRef_givenJmod_thenJmodModulePathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    jmod.setModulePathRef(new Reference("42"));

    // Assert
    Path modulePath = jmod.getModulePath();
    assertNull(modulePath.getDescription());
    assertNull(modulePath.getProject());
    assertNull(modulePath.getRefid());
    assertFalse(modulePath.isReference());
  }

  /**
   * Test {@link Jmod#setModulePathRef(Reference)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) ModulePath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setModulePathRef(Reference)}
   */
  @Test
  public void testSetModulePathRef_thenJmodModulePathProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act
    jmod.setModulePathRef(new Reference("42"));

    // Assert
    Path modulePath = jmod.getModulePath();
    assertNull(modulePath.getDescription());
    assertNull(modulePath.getRefid());
    assertFalse(modulePath.isReference());
    assertSame(project, modulePath.getProject());
  }

  /**
   * Test {@link Jmod#createCommandPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createCommandPath()}
   */
  @Test
  public void testCreateCommandPath_givenJmodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act and Assert
    assertSame(project, jmod.createCommandPath().getProject());
  }

  /**
   * Test {@link Jmod#createCommandPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) CommandPath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createCommandPath()}
   */
  @Test
  public void testCreateCommandPath_givenJmod_thenJmodCommandPathDescriptionIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    Path actualCreateCommandPathResult = jmod.createCommandPath();

    // Assert
    Path commandPath = jmod.getCommandPath();
    assertNull(commandPath.getDescription());
    assertNull(actualCreateCommandPathResult.getProject());
    assertNull(commandPath.getProject());
    assertNull(commandPath.getRefid());
    assertEquals(0, commandPath.size());
    assertFalse(commandPath.isReference());
    assertTrue(commandPath.isEmpty());
  }

  /**
   * Test {@link Jmod#createCommandPath()}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) CommandPath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createCommandPath()}
   */
  @Test
  public void testCreateCommandPath_thenJmodCommandPathIsSystemBootClasspath() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setCommandPath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedCommandPath = jmod.createCommandPath().systemBootClasspath;
    assertSame(expectedCommandPath, jmod.getCommandPath());
  }

  /**
   * Test {@link Jmod#setCommandPath(Path)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) CommandPath is {@link Path#Path(Project)} with project is {@code null} {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setCommandPath(Path)}
   */
  @Test
  public void testSetCommandPath_thenJmodCommandPathIsPathWithProjectIsNullSystemBootClasspath() {
    // Arrange
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    Jmod jmod = new Jmod();
    jmod.setCommandPath(path);
    Path path2 = new Path(null);

    // Act
    jmod.setCommandPath(path2);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    Path expectedCommandPath = path2.systemBootClasspath;
    assertSame(expectedCommandPath, jmod.getCommandPath());
  }

  /**
   * Test {@link Jmod#setCommandPath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Jmod} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setCommandPath(Path)}
   */
  @Test
  public void testSetCommandPath_whenNull_thenJmodRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setCommandPath(Path.systemBootClasspath);

    // Act
    jmod.setCommandPath(null);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Jmod#setCommandPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) CommandPath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setCommandPathRef(Reference)}
   */
  @Test
  public void testSetCommandPathRef_givenJmod_thenJmodCommandPathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    jmod.setCommandPathRef(new Reference("42"));

    // Assert
    Path commandPath = jmod.getCommandPath();
    assertNull(commandPath.getDescription());
    assertNull(commandPath.getProject());
    assertNull(commandPath.getRefid());
    assertFalse(commandPath.isReference());
  }

  /**
   * Test {@link Jmod#setCommandPathRef(Reference)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) CommandPath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setCommandPathRef(Reference)}
   */
  @Test
  public void testSetCommandPathRef_thenJmodCommandPathProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act
    jmod.setCommandPathRef(new Reference("42"));

    // Assert
    Path commandPath = jmod.getCommandPath();
    assertNull(commandPath.getDescription());
    assertNull(commandPath.getRefid());
    assertFalse(commandPath.isReference());
    assertSame(project, commandPath.getProject());
  }

  /**
   * Test {@link Jmod#createConfigPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createConfigPath()}
   */
  @Test
  public void testCreateConfigPath_givenJmodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act and Assert
    assertSame(project, jmod.createConfigPath().getProject());
  }

  /**
   * Test {@link Jmod#createConfigPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) ConfigPath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createConfigPath()}
   */
  @Test
  public void testCreateConfigPath_givenJmod_thenJmodConfigPathDescriptionIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    Path actualCreateConfigPathResult = jmod.createConfigPath();

    // Assert
    Path configPath = jmod.getConfigPath();
    assertNull(configPath.getDescription());
    assertNull(actualCreateConfigPathResult.getProject());
    assertNull(configPath.getProject());
    assertNull(configPath.getRefid());
    assertEquals(0, configPath.size());
    assertFalse(configPath.isReference());
    assertTrue(configPath.isEmpty());
  }

  /**
   * Test {@link Jmod#createConfigPath()}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) ConfigPath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createConfigPath()}
   */
  @Test
  public void testCreateConfigPath_thenJmodConfigPathIsSystemBootClasspath() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setConfigPath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedConfigPath = jmod.createConfigPath().systemBootClasspath;
    assertSame(expectedConfigPath, jmod.getConfigPath());
  }

  /**
   * Test {@link Jmod#setConfigPath(Path)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) ConfigPath is {@link Path#Path(Project)} with project is {@code null} {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setConfigPath(Path)}
   */
  @Test
  public void testSetConfigPath_thenJmodConfigPathIsPathWithProjectIsNullSystemBootClasspath() {
    // Arrange
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    Jmod jmod = new Jmod();
    jmod.setConfigPath(path);
    Path path2 = new Path(null);

    // Act
    jmod.setConfigPath(path2);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    Path expectedConfigPath = path2.systemBootClasspath;
    assertSame(expectedConfigPath, jmod.getConfigPath());
  }

  /**
   * Test {@link Jmod#setConfigPath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Jmod} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setConfigPath(Path)}
   */
  @Test
  public void testSetConfigPath_whenNull_thenJmodRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setConfigPath(Path.systemBootClasspath);

    // Act
    jmod.setConfigPath(null);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Jmod#setConfigPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) ConfigPath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setConfigPathRef(Reference)}
   */
  @Test
  public void testSetConfigPathRef_givenJmod_thenJmodConfigPathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    jmod.setConfigPathRef(new Reference("42"));

    // Assert
    Path configPath = jmod.getConfigPath();
    assertNull(configPath.getDescription());
    assertNull(configPath.getProject());
    assertNull(configPath.getRefid());
    assertFalse(configPath.isReference());
  }

  /**
   * Test {@link Jmod#setConfigPathRef(Reference)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) ConfigPath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setConfigPathRef(Reference)}
   */
  @Test
  public void testSetConfigPathRef_thenJmodConfigPathProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act
    jmod.setConfigPathRef(new Reference("42"));

    // Assert
    Path configPath = jmod.getConfigPath();
    assertNull(configPath.getDescription());
    assertNull(configPath.getRefid());
    assertFalse(configPath.isReference());
    assertSame(project, configPath.getProject());
  }

  /**
   * Test {@link Jmod#createHeaderPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createHeaderPath()}
   */
  @Test
  public void testCreateHeaderPath_givenJmodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act and Assert
    assertSame(project, jmod.createHeaderPath().getProject());
  }

  /**
   * Test {@link Jmod#createHeaderPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) HeaderPath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createHeaderPath()}
   */
  @Test
  public void testCreateHeaderPath_givenJmod_thenJmodHeaderPathDescriptionIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    Path actualCreateHeaderPathResult = jmod.createHeaderPath();

    // Assert
    Path headerPath = jmod.getHeaderPath();
    assertNull(headerPath.getDescription());
    assertNull(actualCreateHeaderPathResult.getProject());
    assertNull(headerPath.getProject());
    assertNull(headerPath.getRefid());
    assertEquals(0, headerPath.size());
    assertFalse(headerPath.isReference());
    assertTrue(headerPath.isEmpty());
  }

  /**
   * Test {@link Jmod#createHeaderPath()}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) HeaderPath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createHeaderPath()}
   */
  @Test
  public void testCreateHeaderPath_thenJmodHeaderPathIsSystemBootClasspath() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setHeaderPath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedHeaderPath = jmod.createHeaderPath().systemBootClasspath;
    assertSame(expectedHeaderPath, jmod.getHeaderPath());
  }

  /**
   * Test {@link Jmod#setHeaderPath(Path)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setHeaderPath(Path)}
   */
  @Test
  public void testSetHeaderPath_givenPathWithProjectIsProjectAddFilesetFileSet() throws BuildException {
    // Arrange
    Project project = new Project();

    Path path = new Path(project);
    path.addFileset(new FileSet());

    Jmod jmod = new Jmod();
    jmod.setHeaderPath(path);
    Path path2 = Path.systemBootClasspath;
    path2.setProject(null);

    // Act
    jmod.setHeaderPath(path2);

    // Assert
    assertSame(project, path2.getProject());
  }

  /**
   * Test {@link Jmod#setHeaderPath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setHeaderPath(Path)}
   */
  @Test
  public void testSetHeaderPath_thenSystemBootClasspathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setHeaderPath(new Path(null));
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    // Act
    jmod.setHeaderPath(path);

    // Assert that nothing has changed
    assertNull(path.getProject());
  }

  /**
   * Test {@link Jmod#setHeaderPath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setHeaderPath(Path)}
   */
  @Test
  public void testSetHeaderPath_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setHeaderPath(new Path(project));
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    // Act
    jmod.setHeaderPath(path);

    // Assert
    assertSame(project, path.getProject());
  }

  /**
   * Test {@link Jmod#setHeaderPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) HeaderPath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setHeaderPathRef(Reference)}
   */
  @Test
  public void testSetHeaderPathRef_givenJmod_thenJmodHeaderPathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    jmod.setHeaderPathRef(new Reference("42"));

    // Assert
    Path headerPath = jmod.getHeaderPath();
    assertNull(headerPath.getDescription());
    assertNull(headerPath.getProject());
    assertNull(headerPath.getRefid());
    assertFalse(headerPath.isReference());
  }

  /**
   * Test {@link Jmod#createLegalPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createLegalPath()}
   */
  @Test
  public void testCreateLegalPath_givenJmodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act and Assert
    assertSame(project, jmod.createLegalPath().getProject());
  }

  /**
   * Test {@link Jmod#createLegalPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) LegalPath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createLegalPath()}
   */
  @Test
  public void testCreateLegalPath_givenJmod_thenJmodLegalPathDescriptionIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    Path actualCreateLegalPathResult = jmod.createLegalPath();

    // Assert
    Path legalPath = jmod.getLegalPath();
    assertNull(legalPath.getDescription());
    assertNull(actualCreateLegalPathResult.getProject());
    assertNull(legalPath.getProject());
    assertNull(legalPath.getRefid());
    assertEquals(0, legalPath.size());
    assertFalse(legalPath.isReference());
    assertTrue(legalPath.isEmpty());
  }

  /**
   * Test {@link Jmod#createLegalPath()}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) LegalPath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createLegalPath()}
   */
  @Test
  public void testCreateLegalPath_thenJmodLegalPathIsSystemBootClasspath() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setLegalPath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedLegalPath = jmod.createLegalPath().systemBootClasspath;
    assertSame(expectedLegalPath, jmod.getLegalPath());
  }

  /**
   * Test {@link Jmod#setLegalPath(Path)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) LegalPath is {@link Path#Path(Project)} with project is {@code null} {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setLegalPath(Path)}
   */
  @Test
  public void testSetLegalPath_thenJmodLegalPathIsPathWithProjectIsNullSystemBootClasspath() {
    // Arrange
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    Jmod jmod = new Jmod();
    jmod.setLegalPath(path);
    Path path2 = new Path(null);

    // Act
    jmod.setLegalPath(path2);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    Path expectedLegalPath = path2.systemBootClasspath;
    assertSame(expectedLegalPath, jmod.getLegalPath());
  }

  /**
   * Test {@link Jmod#setLegalPath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Jmod} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setLegalPath(Path)}
   */
  @Test
  public void testSetLegalPath_whenNull_thenJmodRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setLegalPath(Path.systemBootClasspath);

    // Act
    jmod.setLegalPath(null);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Jmod#setLegalPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) LegalPath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setLegalPathRef(Reference)}
   */
  @Test
  public void testSetLegalPathRef_givenJmodProjectIsProject_thenJmodLegalPathProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act
    jmod.setLegalPathRef(new Reference("42"));

    // Assert
    Path legalPath = jmod.getLegalPath();
    assertNull(legalPath.getDescription());
    assertNull(legalPath.getRefid());
    assertFalse(legalPath.isReference());
    assertSame(project, legalPath.getProject());
  }

  /**
   * Test {@link Jmod#setLegalPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) LegalPath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setLegalPathRef(Reference)}
   */
  @Test
  public void testSetLegalPathRef_givenJmod_thenJmodLegalPathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    jmod.setLegalPathRef(new Reference("42"));

    // Assert
    Path legalPath = jmod.getLegalPath();
    assertNull(legalPath.getDescription());
    assertNull(legalPath.getProject());
    assertNull(legalPath.getRefid());
    assertFalse(legalPath.isReference());
  }

  /**
   * Test {@link Jmod#createNativeLibPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createNativeLibPath()}
   */
  @Test
  public void testCreateNativeLibPath_givenJmodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act and Assert
    assertSame(project, jmod.createNativeLibPath().getProject());
  }

  /**
   * Test {@link Jmod#createNativeLibPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) NativeLibPath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createNativeLibPath()}
   */
  @Test
  public void testCreateNativeLibPath_givenJmod_thenJmodNativeLibPathDescriptionIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    Path actualCreateNativeLibPathResult = jmod.createNativeLibPath();

    // Assert
    Path nativeLibPath = jmod.getNativeLibPath();
    assertNull(nativeLibPath.getDescription());
    assertNull(actualCreateNativeLibPathResult.getProject());
    assertNull(nativeLibPath.getProject());
    assertNull(nativeLibPath.getRefid());
    assertEquals(0, nativeLibPath.size());
    assertFalse(nativeLibPath.isReference());
    assertTrue(nativeLibPath.isEmpty());
  }

  /**
   * Test {@link Jmod#createNativeLibPath()}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) NativeLibPath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createNativeLibPath()}
   */
  @Test
  public void testCreateNativeLibPath_thenJmodNativeLibPathIsSystemBootClasspath() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setNativeLibPath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedNativeLibPath = jmod.createNativeLibPath().systemBootClasspath;
    assertSame(expectedNativeLibPath, jmod.getNativeLibPath());
  }

  /**
   * Test {@link Jmod#setNativeLibPath(Path)}.
   * <p>
   * Method under test: {@link Jmod#setNativeLibPath(Path)}
   */
  @Test
  public void testSetNativeLibPath() {
    // Arrange
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    Jmod jmod = new Jmod();
    jmod.setNativeLibPath(path);
    Path path2 = new Path(null);

    // Act
    jmod.setNativeLibPath(path2);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    Path expectedNativeLibPath = path2.systemBootClasspath;
    assertSame(expectedNativeLibPath, jmod.getNativeLibPath());
  }

  /**
   * Test {@link Jmod#setNativeLibPath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setNativeLibPath(Path)}
   */
  @Test
  public void testSetNativeLibPath_whenNull() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setNativeLibPath(Path.systemBootClasspath);

    // Act
    jmod.setNativeLibPath(null);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Jmod#setNativeLibPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) NativeLibPath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setNativeLibPathRef(Reference)}
   */
  @Test
  public void testSetNativeLibPathRef_givenJmod_thenJmodNativeLibPathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    jmod.setNativeLibPathRef(new Reference("42"));

    // Assert
    Path nativeLibPath = jmod.getNativeLibPath();
    assertNull(nativeLibPath.getDescription());
    assertNull(nativeLibPath.getProject());
    assertNull(nativeLibPath.getRefid());
    assertFalse(nativeLibPath.isReference());
  }

  /**
   * Test {@link Jmod#setNativeLibPathRef(Reference)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) NativeLibPath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setNativeLibPathRef(Reference)}
   */
  @Test
  public void testSetNativeLibPathRef_thenJmodNativeLibPathProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act
    jmod.setNativeLibPathRef(new Reference("42"));

    // Assert
    Path nativeLibPath = jmod.getNativeLibPath();
    assertNull(nativeLibPath.getDescription());
    assertNull(nativeLibPath.getRefid());
    assertFalse(nativeLibPath.isReference());
    assertSame(project, nativeLibPath.getProject());
  }

  /**
   * Test {@link Jmod#createManPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createManPath()}
   */
  @Test
  public void testCreateManPath_givenJmodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act and Assert
    assertSame(project, jmod.createManPath().getProject());
  }

  /**
   * Test {@link Jmod#createManPath()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) ManPath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createManPath()}
   */
  @Test
  public void testCreateManPath_givenJmod_thenJmodManPathDescriptionIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    Path actualCreateManPathResult = jmod.createManPath();

    // Assert
    Path manPath = jmod.getManPath();
    assertNull(manPath.getDescription());
    assertNull(actualCreateManPathResult.getProject());
    assertNull(manPath.getProject());
    assertNull(manPath.getRefid());
    assertEquals(0, manPath.size());
    assertFalse(manPath.isReference());
    assertTrue(manPath.isEmpty());
  }

  /**
   * Test {@link Jmod#createManPath()}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) ManPath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#createManPath()}
   */
  @Test
  public void testCreateManPath_thenJmodManPathIsSystemBootClasspath() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setManPath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedManPath = jmod.createManPath().systemBootClasspath;
    assertSame(expectedManPath, jmod.getManPath());
  }

  /**
   * Test {@link Jmod#setManPath(Path)}.
   * <ul>
   *   <li>Then {@link Jmod} (default constructor) ManPath is {@link Path#Path(Project)} with project is {@code null} {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setManPath(Path)}
   */
  @Test
  public void testSetManPath_thenJmodManPathIsPathWithProjectIsNullSystemBootClasspath() {
    // Arrange
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    Jmod jmod = new Jmod();
    jmod.setManPath(path);
    Path path2 = new Path(null);

    // Act
    jmod.setManPath(path2);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    Path expectedManPath = path2.systemBootClasspath;
    assertSame(expectedManPath, jmod.getManPath());
  }

  /**
   * Test {@link Jmod#setManPath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Jmod} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setManPath(Path)}
   */
  @Test
  public void testSetManPath_whenNull_thenJmodRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setManPath(Path.systemBootClasspath);

    // Act
    jmod.setManPath(null);

    // Assert that nothing has changed
    assertTrue(jmod.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Jmod#setManPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) ManPath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setManPathRef(Reference)}
   */
  @Test
  public void testSetManPathRef_givenJmodProjectIsProject_thenJmodManPathProjectIsProject() {
    // Arrange
    Jmod jmod = new Jmod();
    Project project = new Project();
    jmod.setProject(project);

    // Act
    jmod.setManPathRef(new Reference("42"));

    // Assert
    Path manPath = jmod.getManPath();
    assertNull(manPath.getDescription());
    assertNull(manPath.getRefid());
    assertFalse(manPath.isReference());
    assertSame(project, manPath.getProject());
  }

  /**
   * Test {@link Jmod#setManPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then {@link Jmod} (default constructor) ManPath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#setManPathRef(Reference)}
   */
  @Test
  public void testSetManPathRef_givenJmod_thenJmodManPathProjectIsNull() {
    // Arrange
    Jmod jmod = new Jmod();

    // Act
    jmod.setManPathRef(new Reference("42"));

    // Assert
    Path manPath = jmod.getManPath();
    assertNull(manPath.getDescription());
    assertNull(manPath.getProject());
    assertNull(manPath.getRefid());
    assertFalse(manPath.isReference());
  }

  /**
   * Test {@link Jmod#createVersion()}.
   * <p>
   * Method under test: {@link Jmod#createVersion()}
   */
  @Test
  public void testCreateVersion() {
    // Arrange and Act
    ModuleVersion actualCreateVersionResult = (new Jmod()).createVersion();

    // Assert
    assertNull(actualCreateVersionResult.getBuild());
    assertNull(actualCreateVersionResult.getNumber());
    assertNull(actualCreateVersionResult.getPreRelease());
  }

  /**
   * Test {@link Jmod#createModuleWarning()}.
   * <p>
   * Method under test: {@link Jmod#createModuleWarning()}
   */
  @Test
  public void testCreateModuleWarning() {
    // Arrange, Act and Assert
    assertNull((new Jmod()).createModuleWarning().getReason());
  }

  /**
   * Test {@link Jmod#execute()}.
   * <p>
   * Method under test: {@link Jmod#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setClasspath(new Path(null, "Classpath must contain at least one entry which exists."));
    jmod.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jmod.execute());
  }

  /**
   * Test {@link Jmod#execute()}.
   * <ul>
   *   <li>Given {@link Jmod} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#execute()}
   */
  @Test
  public void testExecute_givenJmod_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Jmod()).execute());
  }

  /**
   * Test {@link Jmod#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jmod.execute());
  }

  /**
   * Test {@link Jmod#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jmod#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException2() throws BuildException {
    // Arrange
    Jmod jmod = new Jmod();
    jmod.setClasspath(new Path(new Project(), "Classpath must contain at least one entry which exists."));
    jmod.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jmod.execute());
  }

  /**
   * Test new {@link Jmod} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Jmod}
   */
  @Test
  public void testNewJmod() {
    // Arrange and Act
    Jmod actualJmod = new Jmod();

    // Assert
    assertNull(actualJmod.getDestFile());
    assertNull(actualJmod.getDescription());
    assertNull(actualJmod.getTaskName());
    assertNull(actualJmod.getTaskType());
    assertNull(actualJmod.getHashModulesPattern());
    assertNull(actualJmod.getMainClass());
    assertNull(actualJmod.getPlatform());
    assertNull(actualJmod.getVersion());
    assertNull(actualJmod.getProject());
    assertNull(actualJmod.getOwningTarget());
    assertNull(actualJmod.getClasspath());
    assertNull(actualJmod.getCommandPath());
    assertNull(actualJmod.getConfigPath());
    assertNull(actualJmod.getHeaderPath());
    assertNull(actualJmod.getLegalPath());
    assertNull(actualJmod.getManPath());
    assertNull(actualJmod.getModulePath());
    assertNull(actualJmod.getNativeLibPath());
    assertTrue(actualJmod.getResolveByDefault());
  }
}
