package org.apache.tools.ant.taskdefs.optional.junitlauncher.confined;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.List;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Environment;
import org.junit.Test;

public class TestDefinitionDiffblueTest {
  /**
   * Test {@link TestDefinition#getIfProperty()}.
   * <p>
   * Method under test: {@link TestDefinition#getIfProperty()}
   */
  @Test
  public void testGetIfProperty() {
    // Arrange, Act and Assert
    assertNull((new SingleTestClass()).getIfProperty());
  }

  /**
   * Test {@link TestDefinition#setIf(String)}.
   * <p>
   * Method under test: {@link TestDefinition#setIf(String)}
   */
  @Test
  public void testSetIf() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setIf("If Property");

    // Assert
    assertEquals("If Property", singleTestClass.getIfProperty());
  }

  /**
   * Test {@link TestDefinition#getUnlessProperty()}.
   * <p>
   * Method under test: {@link TestDefinition#getUnlessProperty()}
   */
  @Test
  public void testGetUnlessProperty() {
    // Arrange, Act and Assert
    assertNull((new SingleTestClass()).getUnlessProperty());
  }

  /**
   * Test {@link TestDefinition#setUnless(String)}.
   * <p>
   * Method under test: {@link TestDefinition#setUnless(String)}
   */
  @Test
  public void testSetUnless() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setUnless("Unless Property");

    // Assert
    assertEquals("Unless Property", singleTestClass.getUnlessProperty());
  }

  /**
   * Test {@link TestDefinition#isHaltOnFailure()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) HaltOnFailure is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#isHaltOnFailure()}
   */
  @Test
  public void testIsHaltOnFailure_givenSingleTestClassHaltOnFailureIsFalse_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setHaltOnFailure(false);

    // Act and Assert
    assertFalse(singleTestClass.isHaltOnFailure());
  }

  /**
   * Test {@link TestDefinition#isHaltOnFailure()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) HaltOnFailure is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#isHaltOnFailure()}
   */
  @Test
  public void testIsHaltOnFailure_givenSingleTestClassHaltOnFailureIsTrue_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setHaltOnFailure(true);

    // Act and Assert
    assertTrue(singleTestClass.isHaltOnFailure());
  }

  /**
   * Test {@link TestDefinition#isHaltOnFailure()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#isHaltOnFailure()}
   */
  @Test
  public void testIsHaltOnFailure_givenSingleTestClass_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SingleTestClass()).isHaltOnFailure());
  }

  /**
   * Test {@link TestDefinition#getHaltOnFailure()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) HaltOnFailure is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getHaltOnFailure()}
   */
  @Test
  public void testGetHaltOnFailure_givenSingleTestClassHaltOnFailureIsFalse_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setHaltOnFailure(false);

    // Act and Assert
    assertFalse(singleTestClass.getHaltOnFailure());
  }

  /**
   * Test {@link TestDefinition#getHaltOnFailure()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) HaltOnFailure is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getHaltOnFailure()}
   */
  @Test
  public void testGetHaltOnFailure_givenSingleTestClassHaltOnFailureIsTrue_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setHaltOnFailure(true);

    // Act and Assert
    assertTrue(singleTestClass.getHaltOnFailure());
  }

  /**
   * Test {@link TestDefinition#getHaltOnFailure()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getHaltOnFailure()}
   */
  @Test
  public void testGetHaltOnFailure_givenSingleTestClass_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new SingleTestClass()).getHaltOnFailure());
  }

  /**
   * Test {@link TestDefinition#setHaltOnFailure(boolean)}.
   * <p>
   * Method under test: {@link TestDefinition#setHaltOnFailure(boolean)}
   */
  @Test
  public void testSetHaltOnFailure() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setHaltOnFailure(true);

    // Assert
    assertTrue(singleTestClass.getHaltOnFailure());
  }

  /**
   * Test {@link TestDefinition#getFailureProperty()}.
   * <p>
   * Method under test: {@link TestDefinition#getFailureProperty()}
   */
  @Test
  public void testGetFailureProperty() {
    // Arrange, Act and Assert
    assertNull((new SingleTestClass()).getFailureProperty());
  }

  /**
   * Test {@link TestDefinition#setFailureProperty(String)}.
   * <p>
   * Method under test: {@link TestDefinition#setFailureProperty(String)}
   */
  @Test
  public void testSetFailureProperty() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setFailureProperty("Failure Property");

    // Assert
    assertEquals("Failure Property", singleTestClass.getFailureProperty());
  }

  /**
   * Test {@link TestDefinition#addConfiguredListener(ListenerDefinition)}.
   * <p>
   * Method under test: {@link TestDefinition#addConfiguredListener(ListenerDefinition)}
   */
  @Test
  public void testAddConfiguredListener() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    ListenerDefinition listener = new ListenerDefinition();

    // Act
    singleTestClass.addConfiguredListener(listener);

    // Assert
    List<ListenerDefinition> listeners = singleTestClass.getListeners();
    assertEquals(1, listeners.size());
    List<ListenerDefinition> listenerDefinitionList = singleTestClass.listeners;
    assertEquals(1, listenerDefinitionList.size());
    assertSame(listener, listeners.get(0));
    assertSame(listener, listenerDefinitionList.get(0));
  }

  /**
   * Test {@link TestDefinition#getListeners()}.
   * <p>
   * Method under test: {@link TestDefinition#getListeners()}
   */
  @Test
  public void testGetListeners() {
    // Arrange, Act and Assert
    assertTrue((new SingleTestClass()).getListeners().isEmpty());
  }

  /**
   * Test {@link TestDefinition#setOutputDir(File)}.
   * <p>
   * Method under test: {@link TestDefinition#setOutputDir(File)}
   */
  @Test
  public void testSetOutputDir() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    File dir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    singleTestClass.setOutputDir(dir);

    // Assert
    assertSame(dir, singleTestClass.getOutputDir());
  }

  /**
   * Test {@link TestDefinition#getOutputDir()}.
   * <p>
   * Method under test: {@link TestDefinition#getOutputDir()}
   */
  @Test
  public void testGetOutputDir() {
    // Arrange, Act and Assert
    assertNull((new SingleTestClass()).getOutputDir());
  }

  /**
   * Test {@link TestDefinition#createFork()}.
   * <p>
   * Method under test: {@link TestDefinition#createFork()}
   */
  @Test
  public void testCreateFork() throws BuildException {
    // Arrange and Act
    ForkDefinition actualCreateForkResult = (new SingleTestClass()).createFork();

    // Assert
    Environment env = actualCreateForkResult.getEnv();
    assertNull(env.getVariables());
    assertNull(actualCreateForkResult.getDir());
    assertNull(actualCreateForkResult.getForkMode());
    assertEquals(-1L, actualCreateForkResult.getTimeout());
    assertTrue(env.getVariablesVector().isEmpty());
  }

  /**
   * Test {@link TestDefinition#getForkDefinition()}.
   * <p>
   * Method under test: {@link TestDefinition#getForkDefinition()}
   */
  @Test
  public void testGetForkDefinition() {
    // Arrange, Act and Assert
    assertNull((new SingleTestClass()).getForkDefinition());
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(project));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenJavaLangObject() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(project));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsAntRefid_whenNull_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("ant.refid:");

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsAntRefid_whenProject_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("ant.refid:");

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(new Project()));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is empty string.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsEmptyString_whenNull_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("");

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@link Boolean#FALSE} toString.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsFalseToString_whenNull_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf(Boolean.FALSE.toString());

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code foo}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsFoo_whenNull_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("foo");

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code no}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsNo_whenNull_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("no");

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code null}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsNull_whenNull_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf(null);

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code off}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsOff_whenNull_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("off");

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code on}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsOn_whenNull_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("on");

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code toString:}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsToString_whenNull_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("toString:");

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code toString:}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsToString_whenProject_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("toString:");

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(new Project()));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@link Boolean#TRUE} toString.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsTrueToString_whenNull_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf(Boolean.TRUE.toString());

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) If is {@code yes}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassIfIsYes_whenNull_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless(null);
    singleTestClass.setIf("yes");

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) Unless is {@code foo}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassUnlessIsFoo_whenNull_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless("foo");
    singleTestClass.setIf(null);

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) Unless is {@code on}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClassUnlessIsOn_whenNull_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setUnless("on");
    singleTestClass.setIf("on");

    // Act and Assert
    assertFalse(singleTestClass.shouldRun(null));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor).</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenSingleTestClass_whenProject_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(new Project()));
  }

  /**
   * Test {@link TestDefinition#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenTarget_whenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(singleTestClass.shouldRun(project));
  }

  /**
   * Test {@link TestDefinition#getIncludeEngines()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) IncludeEngines is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getIncludeEngines()}
   */
  @Test
  public void testGetIncludeEngines_givenSingleTestClassIncludeEnginesIsEmptyString() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setIncludeEngines("");

    // Act and Assert
    assertEquals(0, singleTestClass.getIncludeEngines().length);
  }

  /**
   * Test {@link TestDefinition#getIncludeEngines()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getIncludeEngines()}
   */
  @Test
  public void testGetIncludeEngines_givenSingleTestClass_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new SingleTestClass()).getIncludeEngines().length);
  }

  /**
   * Test {@link TestDefinition#getIncludeEngines()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getIncludeEngines()}
   */
  @Test
  public void testGetIncludeEngines_thenReturnArrayOfStringWithFoo() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setIncludeEngines("foo");

    // Act and Assert
    assertArrayEquals(new String[]{"foo"}, singleTestClass.getIncludeEngines());
  }

  /**
   * Test {@link TestDefinition#setIncludeEngines(String)}.
   * <p>
   * Method under test: {@link TestDefinition#setIncludeEngines(String)}
   */
  @Test
  public void testSetIncludeEngines() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setIncludeEngines("Include Engines");

    // Assert
    assertEquals("Include Engines", singleTestClass.includeEngines);
    assertArrayEquals(new String[]{"Include Engines"}, singleTestClass.getIncludeEngines());
  }

  /**
   * Test {@link TestDefinition#getExcludeEngines()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) ExcludeEngines is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getExcludeEngines()}
   */
  @Test
  public void testGetExcludeEngines_givenSingleTestClassExcludeEnginesIsEmptyString() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setExcludeEngines("");

    // Act and Assert
    assertEquals(0, singleTestClass.getExcludeEngines().length);
  }

  /**
   * Test {@link TestDefinition#getExcludeEngines()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getExcludeEngines()}
   */
  @Test
  public void testGetExcludeEngines_givenSingleTestClass_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new SingleTestClass()).getExcludeEngines().length);
  }

  /**
   * Test {@link TestDefinition#getExcludeEngines()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestDefinition#getExcludeEngines()}
   */
  @Test
  public void testGetExcludeEngines_thenReturnArrayOfStringWithFoo() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setExcludeEngines("foo");

    // Act and Assert
    assertArrayEquals(new String[]{"foo"}, singleTestClass.getExcludeEngines());
  }

  /**
   * Test {@link TestDefinition#setExcludeEngines(String)}.
   * <p>
   * Method under test: {@link TestDefinition#setExcludeEngines(String)}
   */
  @Test
  public void testSetExcludeEngines() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setExcludeEngines("Exclude Engines");

    // Assert
    assertEquals("Exclude Engines", singleTestClass.excludeEngines);
    assertArrayEquals(new String[]{"Exclude Engines"}, singleTestClass.getExcludeEngines());
  }
}
