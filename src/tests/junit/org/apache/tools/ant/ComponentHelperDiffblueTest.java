package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Hashtable;
import java.util.Map;
import org.junit.Test;

public class ComponentHelperDiffblueTest {
  /**
   * Test {@link ComponentHelper#getComponentHelper(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return AntTypeTable size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#getComponentHelper(Project)}
   */
  @Test
  public void testGetComponentHelper_givenJavaLangObject_thenReturnAntTypeTableSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act
    ComponentHelper actualComponentHelper = ComponentHelper.getComponentHelper(project);

    // Assert
    Hashtable<String, AntTypeDefinition> antTypeTable = actualComponentHelper.getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    Hashtable<String, Class<?>> dataTypeDefinitions = actualComponentHelper.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualComponentHelper.getProject().getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertTrue(antTypeTable.containsKey(MagicNames.REFID_PROPERTY_HELPER));
    assertTrue(copyOfDataTypeDefinitions.containsKey(MagicNames.REFID_PROPERTY_HELPER));
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, dataTypeDefinitions.get(MagicNames.REFID_PROPERTY_HELPER));
  }

  /**
   * Test {@link ComponentHelper#getComponentHelper(Project)}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   *   <li>Then return Project References size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#getComponentHelper(Project)}
   */
  @Test
  public void testGetComponentHelper_givenValue_thenReturnProjectReferencesSizeIsTwo() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    Project project2 = ComponentHelper.getComponentHelper(project).getProject();
    Hashtable<String, Object> references = project2.getReferences();
    assertEquals(2, references.size());
    assertEquals("Value", references.get(MagicNames.REFID_PROPERTY_HELPER));
    Map<String, Object> copyOfReferences = project2.getCopyOfReferences();
    assertEquals(2, copyOfReferences.size());
    assertEquals("Value", copyOfReferences.get(MagicNames.REFID_PROPERTY_HELPER));
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(references.containsKey(ComponentHelper.COMPONENT_HELPER_REFERENCE));
    assertTrue(copyOfReferences.containsKey(ComponentHelper.COMPONENT_HELPER_REFERENCE));
  }

  /**
   * Test {@link ComponentHelper#getComponentHelper(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#getComponentHelper(Project)}
   */
  @Test
  public void testGetComponentHelper_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(ComponentHelper.getComponentHelper(null));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ComponentHelper}
   *   <li>{@link ComponentHelper#setNext(ComponentHelper)}
   *   <li>{@link ComponentHelper#setProject(Project)}
   *   <li>{@link ComponentHelper#getAntTypeTable()}
   *   <li>{@link ComponentHelper#getCurrentAntlibUri()}
   *   <li>{@link ComponentHelper#getNext()}
   *   <li>{@link ComponentHelper#getProject()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ComponentHelper actualComponentHelper = new ComponentHelper();
    ComponentHelper next = new ComponentHelper();
    actualComponentHelper.setNext(next);
    Project project = new Project();
    actualComponentHelper.setProject(project);
    Hashtable<String, AntTypeDefinition> actualAntTypeTable = actualComponentHelper.getAntTypeTable();
    String actualCurrentAntlibUri = actualComponentHelper.getCurrentAntlibUri();
    ComponentHelper actualNext = actualComponentHelper.getNext();
    Project actualProject = actualComponentHelper.getProject();

    // Assert
    assertNull(actualCurrentAntlibUri);
    assertTrue(actualAntTypeTable.isEmpty());
    assertSame(next, actualNext);
    assertSame(project, actualProject);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ComponentHelper}
   *   <li>{@link ComponentHelper#setNext(ComponentHelper)}
   *   <li>{@link ComponentHelper#setProject(Project)}
   *   <li>{@link ComponentHelper#getAntTypeTable()}
   *   <li>{@link ComponentHelper#getCurrentAntlibUri()}
   *   <li>{@link ComponentHelper#getNext()}
   *   <li>{@link ComponentHelper#getProject()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters2() {
    // Arrange and Act
    ComponentHelper actualComponentHelper = new ComponentHelper();
    ComponentHelper next = new ComponentHelper();
    actualComponentHelper.setNext(next);
    Project project = new Project();
    actualComponentHelper.setProject(project);
    Hashtable<String, AntTypeDefinition> actualAntTypeTable = actualComponentHelper.getAntTypeTable();
    String actualCurrentAntlibUri = actualComponentHelper.getCurrentAntlibUri();
    ComponentHelper actualNext = actualComponentHelper.getNext();
    Project actualProject = actualComponentHelper.getProject();

    // Assert
    assertNull(actualCurrentAntlibUri);
    assertTrue(actualAntTypeTable.isEmpty());
    assertSame(next, actualNext);
    assertSame(project, actualProject);
  }

  /**
   * Test {@link ComponentHelper#createComponent(String)} with {@code componentName}.
   * <ul>
   *   <li>Then {@link ComponentHelper#ComponentHelper()} AntTypeTable Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#createComponent(String)}
   */
  @Test
  public void testCreateComponentWithComponentName_thenComponentHelperAntTypeTableEmpty() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());

    // Act
    componentHelper.createComponent(ProjectHelper.ANT_CORE_URI);

    // Assert that nothing has changed
    assertTrue(componentHelper.getAntTypeTable().isEmpty());
    assertTrue(componentHelper.getTaskDefinitions().isEmpty());
    Project project = componentHelper.getProject();
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    assertTrue(project.getFilters().isEmpty());
    assertTrue(project.getInheritedProperties().isEmpty());
    assertTrue(project.getTargets().isEmpty());
    assertTrue(project.getTaskDefinitions().isEmpty());
    assertTrue(project.getUserProperties().isEmpty());
    assertTrue(project.getBuildListeners().isEmpty());
  }

  /**
   * Test {@link ComponentHelper#createComponent(String)} with {@code componentName}.
   * <ul>
   *   <li>Then {@link ComponentHelper#ComponentHelper()} AntTypeTable Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#createComponent(String)}
   */
  @Test
  public void testCreateComponentWithComponentName_thenComponentHelperAntTypeTableEmpty2() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());

    // Act
    componentHelper.createComponent(ProjectHelper.ANT_CORE_URI);

    // Assert that nothing has changed
    assertTrue(componentHelper.getAntTypeTable().isEmpty());
    assertTrue(componentHelper.getTaskDefinitions().isEmpty());
    Project project = componentHelper.getProject();
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    assertTrue(project.getFilters().isEmpty());
    assertTrue(project.getInheritedProperties().isEmpty());
    assertTrue(project.getTargets().isEmpty());
    assertTrue(project.getTaskDefinitions().isEmpty());
    assertTrue(project.getUserProperties().isEmpty());
    assertTrue(project.getBuildListeners().isEmpty());
  }

  /**
   * Test {@link ComponentHelper#createComponent(UnknownElement, String, String)} with {@code ue}, {@code ns}, {@code componentType}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#createComponent(UnknownElement, String, String)}
   */
  @Test
  public void testCreateComponentWithUeNsComponentType_whenAnt_core_uri_thenReturnNull() throws BuildException {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());

    // Act and Assert
    assertNull(componentHelper.createComponent(new UnknownElement("Element Name"), "Ns", ProjectHelper.ANT_CORE_URI));
  }

  /**
   * Test {@link ComponentHelper#createComponent(UnknownElement, String, String)} with {@code ue}, {@code ns}, {@code componentType}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#createComponent(UnknownElement, String, String)}
   */
  @Test
  public void testCreateComponentWithUeNsComponentType_whenAnt_core_uri_thenReturnNull2() throws BuildException {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());

    // Act and Assert
    assertNull(componentHelper.createComponent(new UnknownElement("Element Name"), "Ns", ProjectHelper.ANT_CORE_URI));
  }

  /**
   * Test {@link ComponentHelper#getComponentClass(String)}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#getComponentClass(String)}
   */
  @Test
  public void testGetComponentClass_whenAnt_core_uri_thenReturnNull() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());

    // Act and Assert
    assertNull(componentHelper.getComponentClass(ProjectHelper.ANT_CORE_URI));
  }

  /**
   * Test {@link ComponentHelper#getDataTypeDefinitions()}.
   * <ul>
   *   <li>Given {@link ComponentHelper#ComponentHelper()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#getDataTypeDefinitions()}
   */
  @Test
  public void testGetDataTypeDefinitions_givenComponentHelper_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new ComponentHelper()).getDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link ComponentHelper#getDataTypeDefinitions()}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#getDataTypeDefinitions()}
   */
  @Test
  public void testGetDataTypeDefinitions_thenReturnSizeIsOne() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());
    Class<Object> typeClass = Object.class;
    componentHelper.addDataTypeDefinition("Type Name", typeClass);

    // Act
    Hashtable<String, Class<?>> actualDataTypeDefinitions = componentHelper.getDataTypeDefinitions();

    // Assert
    assertEquals(1, actualDataTypeDefinitions.size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, actualDataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link ComponentHelper#addDataTypeDefinition(String, Class)} with {@code typeName}, {@code typeClass}.
   * <p>
   * Method under test: {@link ComponentHelper#addDataTypeDefinition(String, Class)}
   */
  @Test
  public void testAddDataTypeDefinitionWithTypeNameTypeClass() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());
    Class<Object> typeClass = Object.class;

    // Act
    componentHelper.addDataTypeDefinition("Type Name", typeClass);

    // Assert
    Hashtable<String, AntTypeDefinition> antTypeTable = componentHelper.getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    Hashtable<String, Class<?>> dataTypeDefinitions = componentHelper.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    assertTrue(antTypeTable.containsKey("Type Name"));
    Project project = componentHelper.getProject();
    assertTrue(project.getFilters().isEmpty());
    assertTrue(project.getInheritedProperties().isEmpty());
    assertTrue(project.getTargets().isEmpty());
    assertTrue(project.getUserProperties().isEmpty());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, dataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link ComponentHelper#addDataTypeDefinition(String, Class)} with {@code typeName}, {@code typeClass}.
   * <p>
   * Method under test: {@link ComponentHelper#addDataTypeDefinition(String, Class)}
   */
  @Test
  public void testAddDataTypeDefinitionWithTypeNameTypeClass2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(project);
    Class<Object> typeClass = Object.class;

    // Act
    componentHelper.addDataTypeDefinition("Type Name", typeClass);

    // Assert
    Hashtable<String, AntTypeDefinition> antTypeTable = componentHelper.getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    Hashtable<String, Class<?>> dataTypeDefinitions = componentHelper.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    assertTrue(antTypeTable.containsKey("Type Name"));
    Project project2 = componentHelper.getProject();
    assertTrue(project2.getFilters().isEmpty());
    assertTrue(project2.getInheritedProperties().isEmpty());
    assertTrue(project2.getTargets().isEmpty());
    assertTrue(project2.getUserProperties().isEmpty());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, dataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link ComponentHelper#addDataTypeDefinition(String, Class)} with {@code typeName}, {@code typeClass}.
   * <p>
   * Method under test: {@link ComponentHelper#addDataTypeDefinition(String, Class)}
   */
  @Test
  public void testAddDataTypeDefinitionWithTypeNameTypeClass3() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(project);
    Class<Object> typeClass = Object.class;

    // Act
    componentHelper.addDataTypeDefinition("Type Name", typeClass);

    // Assert
    Hashtable<String, AntTypeDefinition> antTypeTable = componentHelper.getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    Hashtable<String, Class<?>> dataTypeDefinitions = componentHelper.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    assertTrue(antTypeTable.containsKey("Type Name"));
    Project project2 = componentHelper.getProject();
    assertTrue(project2.getFilters().isEmpty());
    assertTrue(project2.getInheritedProperties().isEmpty());
    assertTrue(project2.getTargets().isEmpty());
    assertTrue(project2.getUserProperties().isEmpty());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, dataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link ComponentHelper#addDataTypeDefinition(String, Class)} with {@code typeName}, {@code typeClass}.
   * <p>
   * Method under test: {@link ComponentHelper#addDataTypeDefinition(String, Class)}
   */
  @Test
  public void testAddDataTypeDefinitionWithTypeNameTypeClass4() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());
    Class<Object> typeClass = Object.class;

    // Act
    componentHelper.addDataTypeDefinition("Type Name", typeClass);

    // Assert
    Hashtable<String, AntTypeDefinition> antTypeTable = componentHelper.getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    Hashtable<String, Class<?>> dataTypeDefinitions = componentHelper.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    assertTrue(antTypeTable.containsKey("Type Name"));
    Project project = componentHelper.getProject();
    assertTrue(project.getFilters().isEmpty());
    assertTrue(project.getInheritedProperties().isEmpty());
    assertTrue(project.getTargets().isEmpty());
    assertTrue(project.getUserProperties().isEmpty());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, dataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link ComponentHelper#addDataTypeDefinition(String, Class)} with {@code typeName}, {@code typeClass}.
   * <p>
   * Method under test: {@link ComponentHelper#addDataTypeDefinition(String, Class)}
   */
  @Test
  public void testAddDataTypeDefinitionWithTypeNameTypeClass5() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(project);
    Class<Object> typeClass = Object.class;

    // Act
    componentHelper.addDataTypeDefinition("Type Name", typeClass);

    // Assert
    Hashtable<String, AntTypeDefinition> antTypeTable = componentHelper.getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    Hashtable<String, Class<?>> dataTypeDefinitions = componentHelper.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    assertTrue(antTypeTable.containsKey("Type Name"));
    Project project2 = componentHelper.getProject();
    assertTrue(project2.getFilters().isEmpty());
    assertTrue(project2.getInheritedProperties().isEmpty());
    assertTrue(project2.getTargets().isEmpty());
    assertTrue(project2.getUserProperties().isEmpty());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, dataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link ComponentHelper#addDataTypeDefinition(String, Class)} with {@code typeName}, {@code typeClass}.
   * <p>
   * Method under test: {@link ComponentHelper#addDataTypeDefinition(String, Class)}
   */
  @Test
  public void testAddDataTypeDefinitionWithTypeNameTypeClass6() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(project);
    Class<Object> typeClass = Object.class;

    // Act
    componentHelper.addDataTypeDefinition("Type Name", typeClass);

    // Assert
    Hashtable<String, AntTypeDefinition> antTypeTable = componentHelper.getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    Hashtable<String, Class<?>> dataTypeDefinitions = componentHelper.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    assertTrue(antTypeTable.containsKey("Type Name"));
    Project project2 = componentHelper.getProject();
    assertTrue(project2.getFilters().isEmpty());
    assertTrue(project2.getInheritedProperties().isEmpty());
    assertTrue(project2.getTargets().isEmpty());
    assertTrue(project2.getUserProperties().isEmpty());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, dataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link ComponentHelper#createTask(String)}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#createTask(String)}
   */
  @Test
  public void testCreateTask_whenAnt_core_uri_thenReturnNull() throws BuildException {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());

    // Act and Assert
    assertNull(componentHelper.createTask(ProjectHelper.ANT_CORE_URI));
  }

  /**
   * Test {@link ComponentHelper#createDataType(String)}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#createDataType(String)}
   */
  @Test
  public void testCreateDataType_whenAnt_core_uri_thenReturnNull() throws BuildException {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());

    // Act and Assert
    assertNull(componentHelper.createDataType(ProjectHelper.ANT_CORE_URI));
  }

  /**
   * Test {@link ComponentHelper#enterAntLib(String)}.
   * <p>
   * Method under test: {@link ComponentHelper#enterAntLib(String)}
   */
  @Test
  public void testEnterAntLib() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();

    // Act
    componentHelper.enterAntLib("Uri");

    // Assert
    assertEquals("Uri", componentHelper.getCurrentAntlibUri());
  }

  /**
   * Test {@link ComponentHelper#exitAntLib()}.
   * <ul>
   *   <li>Then {@link ComponentHelper#ComponentHelper()} CurrentAntlibUri is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#exitAntLib()}
   */
  @Test
  public void testExitAntLib_thenComponentHelperCurrentAntlibUriIsNull() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.enterAntLib("Uri");

    // Act
    componentHelper.exitAntLib();

    // Assert
    assertNull(componentHelper.getCurrentAntlibUri());
  }

  /**
   * Test {@link ComponentHelper#diagnoseCreationFailure(String, String)}.
   * <ul>
   *   <li>When {@code Problem: failed to create}.</li>
   *   <li>Then return a string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ComponentHelper#diagnoseCreationFailure(String, String)}
   */
  @Test
  public void testDiagnoseCreationFailure_whenProblemFailedToCreate_thenReturnAString() {
    // Arrange
    ComponentHelper componentHelper = new ComponentHelper();
    componentHelper.setProject(new Project());

    // Act and Assert
    assertEquals(
        "Problem: failed to create Type Problem: failed to create \n" + "Cause: The name is undefined.\n"
            + "Action: Check the spelling.\n" + "Action: Check that any custom tasks/types have been declared.\n"
            + "Action: Check that any <presetdef>/<macrodef> declarations have taken place.\n"
            + "No types or tasks have been defined in this namespace yet\n",
        componentHelper.diagnoseCreationFailure("Problem: failed to create ", "Type"));
  }
}
