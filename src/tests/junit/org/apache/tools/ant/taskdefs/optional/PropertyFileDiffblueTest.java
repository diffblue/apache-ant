package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Properties;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.optional.PropertyFile.Entry;
import org.apache.tools.ant.taskdefs.optional.PropertyFile.Entry.Operation;
import org.apache.tools.ant.taskdefs.optional.PropertyFile.Entry.Type;
import org.apache.tools.ant.taskdefs.optional.PropertyFile.Unit;
import org.junit.Test;

public class PropertyFileDiffblueTest {
  /**
   * Test Entry {@link Entry#executeOn(Properties)}.
   * <ul>
   *   <li>Given {@link Entry} (default constructor) Default is {@code 42}.</li>
   *   <li>Then {@link Properties#Properties()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Entry#executeOn(Properties)}
   */
  @Test
  public void testEntryExecuteOn_givenEntryDefaultIs42_thenPropertiesSizeIsOne() throws BuildException {
    // Arrange
    Entry entry = new Entry();
    entry.setKey("42");
    entry.setDefault("42");
    Properties props = new Properties();

    // Act
    entry.executeOn(props);

    // Assert
    assertEquals(1, props.size());
    assertEquals("42", props.get("42"));
  }

  /**
   * Test Entry {@link Entry#executeOn(Properties)}.
   * <ul>
   *   <li>Given {@link Entry} (default constructor) Default is {@code 42}.</li>
   *   <li>Then {@link Properties#Properties()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Entry#executeOn(Properties)}
   */
  @Test
  public void testEntryExecuteOn_givenEntryDefaultIs42_thenPropertiesSizeIsOne2() throws BuildException {
    // Arrange
    Entry entry = new Entry();
    entry.setDefault("42");
    entry.setKey("42");
    entry.setValue("42");
    Properties props = new Properties();

    // Act
    entry.executeOn(props);

    // Assert
    assertEquals(1, props.size());
    assertEquals("42", props.get("42"));
  }

  /**
   * Test Entry {@link Entry#executeOn(Properties)}.
   * <ul>
   *   <li>Given {@link Entry} (default constructor) Default is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Entry#executeOn(Properties)}
   */
  @Test
  public void testEntryExecuteOn_givenEntryDefaultIs42_thenThrowBuildException() throws BuildException {
    // Arrange
    Entry entry = new Entry();
    entry.setDefault("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> entry.executeOn(new Properties()));
  }

  /**
   * Test Entry {@link Entry#executeOn(Properties)}.
   * <ul>
   *   <li>Given {@link Entry} (default constructor) Key is {@code 42}.</li>
   *   <li>Then {@link Properties#Properties()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Entry#executeOn(Properties)}
   */
  @Test
  public void testEntryExecuteOn_givenEntryKeyIs42_thenPropertiesSizeIsOne() throws BuildException {
    // Arrange
    Entry entry = new Entry();
    entry.setKey("42");
    entry.setValue("42");
    Properties props = new Properties();

    // Act
    entry.executeOn(props);

    // Assert
    assertEquals(1, props.size());
    assertEquals("42", props.get("42"));
  }

  /**
   * Test Entry {@link Entry#executeOn(Properties)}.
   * <ul>
   *   <li>Given {@link Entry} (default constructor) Pattern is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Entry#executeOn(Properties)}
   */
  @Test
  public void testEntryExecuteOn_givenEntryPatternIs42_thenThrowBuildException() throws BuildException {
    // Arrange
    Entry entry = new Entry();
    entry.setPattern("42");
    entry.setKey("42");
    entry.setValue("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> entry.executeOn(new Properties()));
  }

  /**
   * Test Entry {@link Entry#executeOn(Properties)}.
   * <ul>
   *   <li>Given {@link Entry} (default constructor) Value is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Entry#executeOn(Properties)}
   */
  @Test
  public void testEntryExecuteOn_givenEntryValueIs42_thenThrowBuildException() throws BuildException {
    // Arrange
    Entry entry = new Entry();
    entry.setValue("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> entry.executeOn(new Properties()));
  }

  /**
   * Test Entry {@link Entry#executeOn(Properties)}.
   * <ul>
   *   <li>Given {@link Entry} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Entry#executeOn(Properties)}
   */
  @Test
  public void testEntryExecuteOn_givenEntry_thenThrowBuildException() throws BuildException {
    // Arrange
    Entry entry = new Entry();

    // Act and Assert
    assertThrows(BuildException.class, () -> entry.executeOn(new Properties()));
  }

  /**
   * Test Entry_Operation {@link Operation#getValues()}.
   * <p>
   * Method under test: {@link Operation#getValues()}
   */
  @Test
  public void testEntry_OperationGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"+", "-", "=", "del"}, (new Operation()).getValues());
  }

  /**
   * Test Entry_Operation new {@link Operation} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Operation}
   */
  @Test
  public void testEntry_OperationNewOperation() {
    // Arrange and Act
    Operation actualOperation = new Operation();

    // Assert
    assertNull(actualOperation.getValue());
    assertEquals(-1, actualOperation.getIndex());
  }

  /**
   * Test Entry_Operation {@link Operation#toOperation(String)}.
   * <ul>
   *   <li>When {@code -}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Operation#toOperation(String)}
   */
  @Test
  public void testEntry_OperationToOperation_whenDash_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, Operation.toOperation("-"));
  }

  /**
   * Test Entry_Operation {@link Operation#toOperation(String)}.
   * <ul>
   *   <li>When {@code del}.</li>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Operation#toOperation(String)}
   */
  @Test
  public void testEntry_OperationToOperation_whenDel_thenReturnThree() {
    // Arrange, Act and Assert
    assertEquals(3, Operation.toOperation("del"));
  }

  /**
   * Test Entry_Operation {@link Operation#toOperation(String)}.
   * <ul>
   *   <li>When {@code Oper}.</li>
   *   <li>Then return two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Operation#toOperation(String)}
   */
  @Test
  public void testEntry_OperationToOperation_whenOper_thenReturnTwo() {
    // Arrange, Act and Assert
    assertEquals(2, Operation.toOperation("Oper"));
  }

  /**
   * Test Entry_Operation {@link Operation#toOperation(String)}.
   * <ul>
   *   <li>When {@code +}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Operation#toOperation(String)}
   */
  @Test
  public void testEntry_OperationToOperation_whenPlusSign_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, Operation.toOperation("+"));
  }

  /**
   * Test Entry_Type {@link Type#getValues()}.
   * <p>
   * Method under test: {@link Type#getValues()}
   */
  @Test
  public void testEntry_TypeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"int", "date", "string"}, (new Type()).getValues());
  }

  /**
   * Test Entry_Type new {@link Type} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Type}
   */
  @Test
  public void testEntry_TypeNewType() {
    // Arrange and Act
    Type actualType = new Type();

    // Assert
    assertNull(actualType.getValue());
    assertEquals(-1, actualType.getIndex());
  }

  /**
   * Test Entry_Type {@link Type#toType(String)}.
   * <ul>
   *   <li>When {@code date}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Type#toType(String)}
   */
  @Test
  public void testEntry_TypeToType_whenDate_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, Type.toType("date"));
  }

  /**
   * Test Entry_Type {@link Type#toType(String)}.
   * <ul>
   *   <li>When {@code int}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Type#toType(String)}
   */
  @Test
  public void testEntry_TypeToType_whenInt_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, Type.toType("int"));
  }

  /**
   * Test Entry_Type {@link Type#toType(String)}.
   * <ul>
   *   <li>When {@code Type}.</li>
   *   <li>Then return two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Type#toType(String)}
   */
  @Test
  public void testEntry_TypeToType_whenType_thenReturnTwo() {
    // Arrange, Act and Assert
    assertEquals(2, Type.toType("Type"));
  }

  /**
   * Test {@link PropertyFile#execute()}.
   * <p>
   * Method under test: {@link PropertyFile#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    PropertyFile propertyFile = new PropertyFile();
    propertyFile.setJDKProperties(false);
    propertyFile.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "Updating property file: ", "foo").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyFile.execute());
  }

  /**
   * Test {@link PropertyFile#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyFile#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    PropertyFile propertyFile = new PropertyFile();
    propertyFile.setProject(project);
    propertyFile.setJDKProperties(false);
    propertyFile.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyFile.execute());
  }

  /**
   * Test {@link PropertyFile#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyFile#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertyFile propertyFile = new PropertyFile();
    propertyFile.setProject(project);
    propertyFile.setJDKProperties(false);
    propertyFile.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyFile.execute());
  }

  /**
   * Test {@link PropertyFile#execute()}.
   * <ul>
   *   <li>Given {@link PropertyFile} (default constructor) JDKProperties is {@code false}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyFile#execute()}
   */
  @Test
  public void testExecute_givenPropertyFileJDKPropertiesIsFalse_thenThrowBuildException() throws BuildException {
    // Arrange
    PropertyFile propertyFile = new PropertyFile();
    propertyFile.setJDKProperties(false);
    propertyFile.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyFile.execute());
  }

  /**
   * Test {@link PropertyFile#execute()}.
   * <ul>
   *   <li>Given {@link PropertyFile} (default constructor) JDKProperties is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyFile#execute()}
   */
  @Test
  public void testExecute_givenPropertyFileJDKPropertiesIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    PropertyFile propertyFile = new PropertyFile();
    propertyFile.setJDKProperties(true);
    propertyFile.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyFile.execute());
  }

  /**
   * Test {@link PropertyFile#execute()}.
   * <ul>
   *   <li>Given {@link PropertyFile} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyFile#execute()}
   */
  @Test
  public void testExecute_givenPropertyFileProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    PropertyFile propertyFile = new PropertyFile();
    propertyFile.setProject(new Project());
    propertyFile.setJDKProperties(false);
    propertyFile.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyFile.execute());
  }

  /**
   * Test {@link PropertyFile#execute()}.
   * <ul>
   *   <li>Given {@link PropertyFile} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyFile#execute()}
   */
  @Test
  public void testExecute_givenPropertyFile_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PropertyFile()).execute());
  }

  /**
   * Test new {@link PropertyFile} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link PropertyFile}
   */
  @Test
  public void testNewPropertyFile() {
    // Arrange and Act
    PropertyFile actualPropertyFile = new PropertyFile();

    // Assert
    Location location = actualPropertyFile.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPropertyFile.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualPropertyFile.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualPropertyFile.getTaskName());
    assertNull(actualPropertyFile.getTaskType());
    assertNull(actualPropertyFile.getProject());
    assertNull(actualPropertyFile.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualPropertyFile, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test Unit {@link Unit#getValues()}.
   * <p>
   * Method under test: {@link Unit#getValues()}
   */
  @Test
  public void testUnitGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"millisecond", "second", "minute", "hour", "day", "week", "month", "year"},
        (new Unit()).getValues());
  }

  /**
   * Test Unit new {@link Unit} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Unit}
   */
  @Test
  public void testUnitNewUnit() {
    // Arrange and Act
    Unit actualUnit = new Unit();

    // Assert
    assertNull(actualUnit.getValue());
    assertEquals(-1, actualUnit.getIndex());
    assertArrayEquals(new String[]{"millisecond", "second", "minute", "hour", "day", "week", "month", "year"},
        actualUnit.getValues());
  }
}
