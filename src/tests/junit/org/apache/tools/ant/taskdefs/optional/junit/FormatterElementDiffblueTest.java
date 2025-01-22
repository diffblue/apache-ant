package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DummyTaskOk;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.optional.junit.FormatterElement.TypeAttribute;
import org.junit.Test;

public class FormatterElementDiffblueTest {
  /**
   * Test {@link FormatterElement#setClassname(String)}.
   * <ul>
   *   <li>Then {@link FormatterElement} (default constructor) Classname is {@link FormatterElement#BRIEF_FORMATTER_CLASS_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#setClassname(String)}
   */
  @Test
  public void testSetClassname_thenFormatterElementClassnameIsBrief_formatter_class_name() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();

    // Act
    formatterElement.setClassname(FormatterElement.BRIEF_FORMATTER_CLASS_NAME);

    // Assert
    assertEquals(".txt", formatterElement.getExtension());
    assertEquals(FormatterElement.BRIEF_FORMATTER_CLASS_NAME, formatterElement.getClassname());
  }

  /**
   * Test {@link FormatterElement#setClassname(String)}.
   * <ul>
   *   <li>Then {@link FormatterElement} (default constructor) Classname is {@link FormatterElement#PLAIN_FORMATTER_CLASS_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#setClassname(String)}
   */
  @Test
  public void testSetClassname_thenFormatterElementClassnameIsPlain_formatter_class_name() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();

    // Act
    formatterElement.setClassname(FormatterElement.PLAIN_FORMATTER_CLASS_NAME);

    // Assert
    assertEquals(".txt", formatterElement.getExtension());
    assertEquals(FormatterElement.PLAIN_FORMATTER_CLASS_NAME, formatterElement.getClassname());
  }

  /**
   * Test {@link FormatterElement#setClassname(String)}.
   * <ul>
   *   <li>When {@code Classname}.</li>
   *   <li>Then {@link FormatterElement} (default constructor) Classname is {@code Classname}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#setClassname(String)}
   */
  @Test
  public void testSetClassname_whenClassname_thenFormatterElementClassnameIsClassname() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();

    // Act
    formatterElement.setClassname("Classname");

    // Assert
    assertEquals("Classname", formatterElement.getClassname());
    assertNull(formatterElement.getExtension());
  }

  /**
   * Test {@link FormatterElement#setClassname(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link FormatterElement} (default constructor) Classname is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#setClassname(String)}
   */
  @Test
  public void testSetClassname_whenNull_thenFormatterElementClassnameIsNull() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();

    // Act
    formatterElement.setClassname(null);

    // Assert that nothing has changed
    assertNull(formatterElement.getClassname());
    assertNull(formatterElement.getExtension());
  }

  /**
   * Test {@link FormatterElement#setClassname(String)}.
   * <ul>
   *   <li>When {@link FormatterElement#XML_FORMATTER_CLASS_NAME}.</li>
   *   <li>Then {@link FormatterElement} (default constructor) Extension is {@code .xml}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#setClassname(String)}
   */
  @Test
  public void testSetClassname_whenXml_formatter_class_name_thenFormatterElementExtensionIsXml() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();

    // Act
    formatterElement.setClassname(FormatterElement.XML_FORMATTER_CLASS_NAME);

    // Assert
    assertEquals(".xml", formatterElement.getExtension());
    assertEquals(FormatterElement.XML_FORMATTER_CLASS_NAME, formatterElement.getClassname());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link FormatterElement#setExtension(String)}
   *   <li>{@link FormatterElement#setIf(Object)}
   *   <li>{@link FormatterElement#setOutfile(File)}
   *   <li>{@link FormatterElement#setProject(Project)}
   *   <li>{@link FormatterElement#setUnless(Object)}
   *   <li>{@link FormatterElement#setUseFile(boolean)}
   *   <li>{@link FormatterElement#getClassname()}
   *   <li>{@link FormatterElement#getExtension()}
   *   <li>{@link FormatterElement#getUseFile()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();

    // Act
    formatterElement.setExtension("Ext");
    formatterElement.setIf((Object) "If Cond");
    formatterElement.setOutfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    formatterElement.setProject(new Project());
    formatterElement.setUnless((Object) "Unless Cond");
    formatterElement.setUseFile(true);
    String actualClassname = formatterElement.getClassname();
    String actualExtension = formatterElement.getExtension();

    // Assert
    assertEquals("Ext", actualExtension);
    assertNull(actualClassname);
    assertTrue(formatterElement.getUseFile());
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIs42_thenReturnFalse() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "42");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsAntRefid_thenReturnFalse() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "ant.refid:");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>When {@link DummyTaskOk} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsAntRefid_whenDummyTaskOkProjectIsProject() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "ant.refid:");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(new Project());

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsEmptyString_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@link Boolean#FALSE} toString.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsFalseToString_thenReturnFalse() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) Boolean.FALSE.toString());

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code no}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsNo_thenReturnFalse() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "no");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code off}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsOff_thenReturnFalse() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "off");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code on}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsOn_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "on");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code toString:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsToString_thenReturnFalse() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "toString:");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code toString:}.</li>
   *   <li>When {@link DummyTaskOk} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsToString_whenDummyTaskOkProjectIsProject() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "toString:");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(new Project());

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsTrueToString_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) Boolean.TRUE.toString());

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsTrue_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf(true);

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) If is {@code yes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementIfIsYes_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) "yes");

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) Unless is {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementUnlessIs42_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) "42");
    formatterElement.setIf((Object) null);

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) Unless is one.</li>
   *   <li>When {@link DummyTaskOk} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementUnlessIsOne_whenDummyTaskOkProjectIsProject() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless(1);
    formatterElement.setIf((Object) null);

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(new Project());

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor) Unless is {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElementUnlessIsTrue_thenReturnFalse() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless(true);
    formatterElement.setIf(true);

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(null);

    // Act and Assert
    assertFalse(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link FormatterElement} (default constructor).</li>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenFormatterElement_whenTaskAdapter_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();

    // Act and Assert
    assertTrue(formatterElement.shouldUse(new TaskAdapter()));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link DummyTaskOk} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenJavaLangObject_whenDummyTaskOkProjectIsProject_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) null);

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(project);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) null);

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(project);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) null);

    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(project);

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test {@link FormatterElement#shouldUse(Task)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link DummyTaskOk} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FormatterElement#shouldUse(Task)}
   */
  @Test
  public void testShouldUse_givenProject_whenDummyTaskOkProjectIsProject_thenReturnTrue() {
    // Arrange
    FormatterElement formatterElement = new FormatterElement();
    formatterElement.setUnless((Object) null);
    formatterElement.setIf((Object) null);

    DummyTaskOk t = new DummyTaskOk();
    t.setProject(new Project());

    // Act and Assert
    assertTrue(formatterElement.shouldUse(t));
  }

  /**
   * Test new {@link FormatterElement} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FormatterElement}
   */
  @Test
  public void testNewFormatterElement() {
    // Arrange and Act
    FormatterElement actualFormatterElement = new FormatterElement();

    // Assert
    assertNull(actualFormatterElement.getClassname());
    assertNull(actualFormatterElement.getExtension());
    assertTrue(actualFormatterElement.getUseFile());
  }

  /**
   * Test TypeAttribute {@link TypeAttribute#getValues()}.
   * <p>
   * Method under test: {@link TypeAttribute#getValues()}
   */
  @Test
  public void testTypeAttributeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"plain", "xml", "brief", XMLConstants.FAILURE}, (new TypeAttribute()).getValues());
  }

  /**
   * Test TypeAttribute new {@link TypeAttribute} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TypeAttribute}
   */
  @Test
  public void testTypeAttributeNewTypeAttribute() {
    // Arrange and Act
    TypeAttribute actualTypeAttribute = new TypeAttribute();

    // Assert
    assertNull(actualTypeAttribute.getValue());
    assertEquals(-1, actualTypeAttribute.getIndex());
  }
}
