package org.apache.tools.ant.taskdefs.optional.junitlauncher.confined;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.optional.junitlauncher.confined.ListenerDefinition.ListenerType;
import org.junit.Test;

public class ListenerDefinitionDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ListenerDefinition}
   *   <li>{@link ListenerDefinition#setClassName(String)}
   *   <li>{@link ListenerDefinition#setExtension(String)}
   *   <li>{@link ListenerDefinition#setIf(String)}
   *   <li>{@link ListenerDefinition#setOutputDir(File)}
   *   <li>{@link ListenerDefinition#setResultFile(String)}
   *   <li>{@link ListenerDefinition#setSendSysErr(boolean)}
   *   <li>{@link ListenerDefinition#setSendSysOut(boolean)}
   *   <li>{@link ListenerDefinition#setUnless(String)}
   *   <li>{@link ListenerDefinition#setUseLegacyReportingName(boolean)}
   *   <li>{@link ListenerDefinition#getClassName()}
   *   <li>{@link ListenerDefinition#getExtension()}
   *   <li>{@link ListenerDefinition#getIfProperty()}
   *   <li>{@link ListenerDefinition#getOutputDir()}
   *   <li>{@link ListenerDefinition#getResultFile()}
   *   <li>{@link ListenerDefinition#getUnlessProperty()}
   *   <li>{@link ListenerDefinition#isUseLegacyReportingName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ListenerDefinition actualListenerDefinition = new ListenerDefinition();
    actualListenerDefinition.setClassName("Class Name");
    actualListenerDefinition.setExtension("Extension");
    actualListenerDefinition.setIf("If Property");
    File dir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualListenerDefinition.setOutputDir(dir);
    actualListenerDefinition.setResultFile("foo.txt");
    actualListenerDefinition.setSendSysErr(true);
    actualListenerDefinition.setSendSysOut(true);
    actualListenerDefinition.setUnless("Unless Property");
    actualListenerDefinition.setUseLegacyReportingName(true);
    String actualClassName = actualListenerDefinition.getClassName();
    String actualExtension = actualListenerDefinition.getExtension();
    String actualIfProperty = actualListenerDefinition.getIfProperty();
    File actualOutputDir = actualListenerDefinition.getOutputDir();
    String actualResultFile = actualListenerDefinition.getResultFile();
    String actualUnlessProperty = actualListenerDefinition.getUnlessProperty();

    // Assert
    assertEquals("Class Name", actualClassName);
    assertEquals("Extension", actualExtension);
    assertEquals("If Property", actualIfProperty);
    assertEquals("Unless Property", actualUnlessProperty);
    assertEquals("foo.txt", actualResultFile);
    assertTrue(actualListenerDefinition.isUseLegacyReportingName());
    assertSame(dir, actualOutputDir);
  }

  /**
   * Test ListenerType {@link ListenerType#getValues()}.
   * <p>
   * Method under test: {@link ListenerType#getValues()}
   */
  @Test
  public void testListenerTypeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"legacy-plain", "legacy-brief", "legacy-xml"}, (new ListenerType()).getValues());
  }

  /**
   * Test ListenerType new {@link ListenerType} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ListenerType}
   */
  @Test
  public void testListenerTypeNewListenerType() {
    // Arrange and Act
    ListenerType actualListenerType = new ListenerType();

    // Assert
    assertNull(actualListenerType.getValue());
    assertEquals(-1, actualListenerType.getIndex());
  }

  /**
   * Test {@link ListenerDefinition#shouldSendSysOut()}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) SendSysOut is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldSendSysOut()}
   */
  @Test
  public void testShouldSendSysOut_givenListenerDefinitionSendSysOutIsTrue_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setSendSysOut(true);

    // Act and Assert
    assertTrue(listenerDefinition.shouldSendSysOut());
  }

  /**
   * Test {@link ListenerDefinition#shouldSendSysOut()}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldSendSysOut()}
   */
  @Test
  public void testShouldSendSysOut_givenListenerDefinition_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new ListenerDefinition()).shouldSendSysOut());
  }

  /**
   * Test {@link ListenerDefinition#shouldSendSysErr()}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) SendSysErr is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldSendSysErr()}
   */
  @Test
  public void testShouldSendSysErr_givenListenerDefinitionSendSysErrIsTrue_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setSendSysErr(true);

    // Act and Assert
    assertTrue(listenerDefinition.shouldSendSysErr());
  }

  /**
   * Test {@link ListenerDefinition#shouldSendSysErr()}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldSendSysErr()}
   */
  @Test
  public void testShouldSendSysErr_givenListenerDefinition_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new ListenerDefinition()).shouldSendSysErr());
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(project));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenJavaLangObject() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(project));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsAntRefid_whenNull_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("ant.refid:");
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsAntRefid_whenProject_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("ant.refid:");
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(new Project()));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@link Boolean#FALSE} toString.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsFalseToString_whenNull_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf(Boolean.FALSE.toString());
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code foo}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsFoo_whenNull_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("foo");
    listenerDefinition.setUnless(null);

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code no}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsNo_whenNull_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("no");
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code null}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsNull_whenNull_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf(null);
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code off}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsOff_whenNull_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("off");
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code on}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsOn_whenNull_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("on");
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code toString:}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsToString_whenNull_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("toString:");
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code toString:}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsToString_whenProject_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("toString:");
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(new Project()));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@link Boolean#TRUE} toString.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsTrueToString_whenNull_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf(Boolean.TRUE.toString());
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) If is {@code yes}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionIfIsYes_whenNull_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf("yes");
    listenerDefinition.setUnless("foo");

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) Unless is empty string.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionUnlessIsEmptyString_whenNull_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf(null);
    listenerDefinition.setUnless("");

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) Unless is {@code null}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionUnlessIsNull_whenNull_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf(null);
    listenerDefinition.setUnless(null);

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor) Unless is {@code on}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinitionUnlessIsOn_whenNull_thenReturnFalse() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();
    listenerDefinition.setIf(null);
    listenerDefinition.setUnless("on");

    // Act and Assert
    assertFalse(listenerDefinition.shouldUse(null));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link ListenerDefinition} (default constructor).</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenListenerDefinition_whenProject_thenReturnTrue() {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(new Project()));
  }

  /**
   * Test {@link ListenerDefinition#shouldUse(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ListenerDefinition#shouldUse(Project)}
   */
  @Test
  public void testShouldUse_givenTarget_whenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    ListenerDefinition listenerDefinition = new ListenerDefinition();

    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(listenerDefinition.shouldUse(project));
  }
}
