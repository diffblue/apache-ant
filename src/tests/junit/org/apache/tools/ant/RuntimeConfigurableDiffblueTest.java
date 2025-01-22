package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Hashtable;
import org.apache.tools.ant.IntrospectionHelper.Creator;
import org.junit.Test;
import org.xml.sax.AttributeList;
import org.xml.sax.helpers.AttributeListImpl;

public class RuntimeConfigurableDiffblueTest {
  /**
   * Test {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)}.
   * <ul>
   *   <li>When {@code Proxy}.</li>
   *   <li>Then return Text toString is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)}
   */
  @Test
  public void testNewRuntimeConfigurable_whenProxy_thenReturnTextToStringIsEmptyString() {
    // Arrange and Act
    RuntimeConfigurable actualRuntimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Assert
    assertEquals("", actualRuntimeConfigurable.getText().toString());
    assertEquals("Element Tag", actualRuntimeConfigurable.getElementTag());
    assertEquals("Proxy", actualRuntimeConfigurable.getProxy());
    assertNull(actualRuntimeConfigurable.getId());
    assertNull(actualRuntimeConfigurable.getPolyType());
    assertNull(actualRuntimeConfigurable.getAttributes());
    assertTrue(actualRuntimeConfigurable.getAttributeMap().isEmpty());
  }

  /**
   * Test {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)}.
   * <ul>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return Proxy is {@link TaskAdapter#TaskAdapter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)}
   */
  @Test
  public void testNewRuntimeConfigurable_whenTaskAdapter_thenReturnProxyIsTaskAdapter() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();

    // Act and Assert
    assertSame(taskAdapter, (new RuntimeConfigurable(taskAdapter, "Element Tag")).getProxy());
  }

  /**
   * Test {@link RuntimeConfigurable#isEnabled(UnknownElement)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#isEnabled(UnknownElement)}
   */
  @Test
  public void testIsEnabled() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act and Assert
    assertTrue(runtimeConfigurable.isEnabled(new UnknownElement("Element Name")));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttributes(new AttributeListImpl());

    // Assert
    AttributeList attributes = runtimeConfigurable.getAttributes();
    assertTrue(attributes instanceof AttributeListImpl);
    assertNull(runtimeConfigurable.getPolyType());
    assertEquals(0, attributes.getLength());
    assertTrue(runtimeConfigurable.getAttributeMap().isEmpty());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes2() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    AttributeListImpl attributes = new AttributeListImpl();
    attributes.addAttribute("foo", "foo", "foo");

    // Act
    runtimeConfigurable.setAttributes(attributes);

    // Assert
    AttributeList attributes2 = runtimeConfigurable.getAttributes();
    assertTrue(attributes2 instanceof AttributeListImpl);
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("foo", attributeMap.get("foo"));
    assertNull(runtimeConfigurable.getPolyType());
    assertEquals(1, attributes2.getLength());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes3() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    AttributeListImpl attributes = new AttributeListImpl();
    attributes.addAttribute(ProjectHelper.ANT_TYPE, ProjectHelper.ANT_TYPE, ProjectHelper.ANT_TYPE);
    attributes.addAttribute("foo", "foo", "foo");

    // Act
    runtimeConfigurable.setAttributes(attributes);

    // Assert
    AttributeList attributes2 = runtimeConfigurable.getAttributes();
    assertTrue(attributes2 instanceof AttributeListImpl);
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("foo", attributeMap.get("foo"));
    assertEquals(2, attributes2.getLength());
    assertEquals(ProjectHelper.ANT_TYPE, runtimeConfigurable.getPolyType());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes4() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    AttributeListImpl attributes = new AttributeListImpl();
    attributes.addAttribute(":", "foo", "foo");

    // Act
    runtimeConfigurable.setAttributes(attributes);

    // Assert
    AttributeList attributes2 = runtimeConfigurable.getAttributes();
    assertTrue(attributes2 instanceof AttributeListImpl);
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("foo", attributeMap.get(":"));
    assertNull(runtimeConfigurable.getPolyType());
    assertEquals(1, attributes2.getLength());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes5() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    AttributeListImpl attributes = new AttributeListImpl();
    attributes.addAttribute("refid", "foo", "foo");

    // Act
    runtimeConfigurable.setAttributes(attributes);

    // Assert
    AttributeList attributes2 = runtimeConfigurable.getAttributes();
    assertTrue(attributes2 instanceof AttributeListImpl);
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("foo", attributeMap.get("refid"));
    assertNull(runtimeConfigurable.getPolyType());
    assertEquals(1, attributes2.getLength());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes6() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    AttributeListImpl attributes = new AttributeListImpl();
    attributes.addAttribute("refid", "refid", "refid");
    attributes.addAttribute(ProjectHelper.ANT_TYPE, ProjectHelper.ANT_TYPE, ProjectHelper.ANT_TYPE);
    attributes.addAttribute("foo", "foo", "foo");

    // Act
    runtimeConfigurable.setAttributes(attributes);

    // Assert
    AttributeList attributes2 = runtimeConfigurable.getAttributes();
    assertTrue(attributes2 instanceof AttributeListImpl);
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(2, attributeMap.size());
    assertEquals("refid", attributeMap.get("refid"));
    assertEquals(3, attributes2.getLength());
    assertTrue(attributeMap.containsKey("foo"));
    assertEquals(ProjectHelper.ANT_TYPE, runtimeConfigurable.getPolyType());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes7() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    AttributeListImpl attributes = new AttributeListImpl();
    attributes.addAttribute(":", ":", ":");
    attributes.addAttribute("refid", "refid", "refid");
    attributes.addAttribute(ProjectHelper.ANT_TYPE, ProjectHelper.ANT_TYPE, ProjectHelper.ANT_TYPE);
    attributes.addAttribute("foo", "foo", "foo");

    // Act
    runtimeConfigurable.setAttributes(attributes);

    // Assert
    AttributeList attributes2 = runtimeConfigurable.getAttributes();
    assertTrue(attributes2 instanceof AttributeListImpl);
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(3, attributeMap.size());
    assertEquals(":", attributeMap.get(":"));
    assertEquals("refid", attributeMap.get("refid"));
    assertEquals(4, attributes2.getLength());
    assertTrue(attributeMap.containsKey("foo"));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <ul>
   *   <li>Given {@code id}.</li>
   *   <li>Then {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)} with {@code Proxy} and {@code Element Tag} Id is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes_givenId_thenRuntimeConfigurableWithProxyAndElementTagIdIsFoo() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    AttributeListImpl attributes = new AttributeListImpl();
    attributes.addAttribute("id", "foo", "foo");

    // Act
    runtimeConfigurable.setAttributes(attributes);

    // Assert
    AttributeList attributes2 = runtimeConfigurable.getAttributes();
    assertTrue(attributes2 instanceof AttributeListImpl);
    assertEquals("foo", runtimeConfigurable.getId());
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("foo", attributeMap.get("id"));
    assertEquals(1, attributes2.getLength());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttributes(AttributeList)}.
   * <ul>
   *   <li>When {@link AttributeListImpl#addAttribute(String, String, String)} with {@link ProjectHelper#ANT_TYPE} and {@link ProjectHelper#ANT_TYPE} and {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttributes(AttributeList)}
   */
  @Test
  public void testSetAttributes_whenAddAttributeWithAnt_typeAndAnt_typeAndNull() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    AttributeListImpl attributes = new AttributeListImpl();
    attributes.addAttribute(ProjectHelper.ANT_TYPE, ProjectHelper.ANT_TYPE, null);
    attributes.addAttribute("foo", "foo", "foo");

    // Act
    runtimeConfigurable.setAttributes(attributes);

    // Assert
    AttributeList attributes2 = runtimeConfigurable.getAttributes();
    assertTrue(attributes2 instanceof AttributeListImpl);
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("foo", attributeMap.get("foo"));
    assertNull(runtimeConfigurable.getPolyType());
    assertEquals(2, attributes2.getLength());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, Object)} with {@code String}, {@code Object}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, Object)}
   */
  @Test
  public void testSetAttributeWithStringObject() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute("Name", (Object) "Value");

    // Assert
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("Value", attributeMap.get("Name"));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, Object)} with {@code String}, {@code Object}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, Object)}
   */
  @Test
  public void testSetAttributeWithStringObject2() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute(ProjectHelper.ANT_TYPE, (Object) "Value");

    // Assert
    assertEquals("Value", runtimeConfigurable.getPolyType());
    assertNull(runtimeConfigurable.getId());
    assertTrue(runtimeConfigurable.getAttributeMap().isEmpty());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, Object)} with {@code String}, {@code Object}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, Object)}
   */
  @Test
  public void testSetAttributeWithStringObject3() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute("refid", (Object) "Value");

    // Assert
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("Value", attributeMap.get("refid"));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, Object)} with {@code String}, {@code Object}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, Object)}
   */
  @Test
  public void testSetAttributeWithStringObject4() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute("id", (Object) "Value");

    // Assert
    assertEquals("Value", runtimeConfigurable.getId());
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("Value", attributeMap.get("id"));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, Object)} with {@code String}, {@code Object}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, Object)}
   */
  @Test
  public void testSetAttributeWithStringObject5() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute(ProjectHelper.ANT_TYPE, (Object) null);

    // Assert that nothing has changed
    assertTrue(runtimeConfigurable.getAttributeMap().isEmpty());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, Object)} with {@code String}, {@code Object}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, Object)}
   */
  @Test
  public void testSetAttributeWithStringObject6() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute("id", (Object) null);

    // Assert that nothing has changed
    assertNull(runtimeConfigurable.getId());
    assertNull(runtimeConfigurable.getPolyType());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, String)} with {@code String}, {@code String}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, String)}
   */
  @Test
  public void testSetAttributeWithStringString() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute("Name", "42");

    // Assert
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("42", attributeMap.get("Name"));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, String)} with {@code String}, {@code String}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, String)}
   */
  @Test
  public void testSetAttributeWithStringString2() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute(":", "42");

    // Assert
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("42", attributeMap.get(":"));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, String)} with {@code String}, {@code String}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, String)}
   */
  @Test
  public void testSetAttributeWithStringString3() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute(ProjectHelper.ANT_TYPE, "42");

    // Assert
    assertEquals("42", runtimeConfigurable.getPolyType());
    assertNull(runtimeConfigurable.getId());
    assertTrue(runtimeConfigurable.getAttributeMap().isEmpty());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, String)} with {@code String}, {@code String}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, String)}
   */
  @Test
  public void testSetAttributeWithStringString4() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute("refid", "42");

    // Assert
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("42", attributeMap.get("refid"));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, String)} with {@code String}, {@code String}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, String)}
   */
  @Test
  public void testSetAttributeWithStringString5() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute("id", "42");

    // Assert
    assertEquals("42", runtimeConfigurable.getId());
    Hashtable<String, Object> attributeMap = runtimeConfigurable.getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("42", attributeMap.get("id"));
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, String)} with {@code String}, {@code String}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, String)}
   */
  @Test
  public void testSetAttributeWithStringString6() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute(ProjectHelper.ANT_TYPE, (String) null);

    // Assert that nothing has changed
    assertTrue(runtimeConfigurable.getAttributeMap().isEmpty());
  }

  /**
   * Test {@link RuntimeConfigurable#setAttribute(String, String)} with {@code String}, {@code String}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#setAttribute(String, String)}
   */
  @Test
  public void testSetAttributeWithStringString7() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setAttribute("id", (String) null);

    // Assert that nothing has changed
    assertNull(runtimeConfigurable.getId());
    assertNull(runtimeConfigurable.getPolyType());
  }

  /**
   * Test {@link RuntimeConfigurable#removeAttribute(String)}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#removeAttribute(String)}
   */
  @Test
  public void testRemoveAttribute() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.setAttribute("Name", "42");

    // Act
    runtimeConfigurable.removeAttribute("Name");

    // Assert
    assertTrue(runtimeConfigurable.getAttributeMap().isEmpty());
  }

  /**
   * Test {@link RuntimeConfigurable#getAttributeMap()}.
   * <ul>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#getAttributeMap()}
   */
  @Test
  public void testGetAttributeMap_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new RuntimeConfigurable("Proxy", "Element Tag")).getAttributeMap().isEmpty());
  }

  /**
   * Test {@link RuntimeConfigurable#getAttributeMap()}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#getAttributeMap()}
   */
  @Test
  public void testGetAttributeMap_thenReturnSizeIsOne() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.setAttribute("Name", "42");

    // Act
    Hashtable<String, Object> actualAttributeMap = runtimeConfigurable.getAttributeMap();

    // Assert
    assertEquals(1, actualAttributeMap.size());
    assertEquals("42", actualAttributeMap.get("Name"));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link RuntimeConfigurable#setElementTag(String)}
   *   <li>{@link RuntimeConfigurable#setPolyType(String)}
   *   <li>{@link RuntimeConfigurable#setCreator(Creator)}
   *   <li>{@link RuntimeConfigurable#getAttributes()}
   *   <li>{@link RuntimeConfigurable#getElementTag()}
   *   <li>{@link RuntimeConfigurable#getId()}
   *   <li>{@link RuntimeConfigurable#getPolyType()}
   *   <li>{@link RuntimeConfigurable#getProxy()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.setElementTag("Element Tag");
    runtimeConfigurable.setPolyType("Poly Type");
    runtimeConfigurable.setCreator(null);
    AttributeList actualAttributes = runtimeConfigurable.getAttributes();
    String actualElementTag = runtimeConfigurable.getElementTag();
    String actualId = runtimeConfigurable.getId();
    String actualPolyType = runtimeConfigurable.getPolyType();

    // Assert
    assertEquals("Element Tag", actualElementTag);
    assertEquals("Poly Type", actualPolyType);
    assertEquals("Proxy", runtimeConfigurable.getProxy());
    assertNull(actualId);
    assertNull(actualAttributes);
  }

  /**
   * Test {@link RuntimeConfigurable#getChild(int)}.
   * <ul>
   *   <li>Then return {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)} with {@code Proxy} and {@code Element Tag}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#getChild(int)}
   */
  @Test
  public void testGetChild_thenReturnRuntimeConfigurableWithProxyAndElementTag() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.addChild(new RuntimeConfigurable("Proxy", "Element Tag"));
    RuntimeConfigurable child = new RuntimeConfigurable("Proxy", "Element Tag");

    runtimeConfigurable.addChild(child);

    // Act and Assert
    assertSame(child, runtimeConfigurable.getChild(1));
  }

  /**
   * Test {@link RuntimeConfigurable#addText(char[], int, int)} with {@code buf}, {@code start}, {@code count}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#addText(char[], int, int)}
   */
  @Test
  public void testAddTextWithBufStartCount() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.addText("A:A:".toCharArray(), 1, 3);

    // Assert
    assertEquals(":A:", runtimeConfigurable.getText().toString());
  }

  /**
   * Test {@link RuntimeConfigurable#addText(char[], int, int)} with {@code buf}, {@code start}, {@code count}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#addText(char[], int, int)}
   */
  @Test
  public void testAddTextWithBufStartCount2() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.addText("Data");

    // Act
    runtimeConfigurable.addText("A:A:".toCharArray(), 1, 3);

    // Assert
    assertEquals("Data:A:", runtimeConfigurable.getText().toString());
  }

  /**
   * Test {@link RuntimeConfigurable#addText(char[], int, int)} with {@code buf}, {@code start}, {@code count}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#addText(char[], int, int)}
   */
  @Test
  public void testAddTextWithBufStartCount3() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.addText("A:A:".toCharArray(), 1, 0);

    // Assert that nothing has changed
    assertEquals("", runtimeConfigurable.getText().toString());
  }

  /**
   * Test {@link RuntimeConfigurable#addText(String)} with {@code data}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#addText(String)}
   */
  @Test
  public void testAddTextWithData() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.addText("Data");

    // Assert
    assertEquals("Data", runtimeConfigurable.getText().toString());
  }

  /**
   * Test {@link RuntimeConfigurable#addText(String)} with {@code data}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#addText(String)}
   */
  @Test
  public void testAddTextWithData2() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.addText("Data");

    // Act
    runtimeConfigurable.addText("Data");

    // Assert
    assertEquals("DataData", runtimeConfigurable.getText().toString());
  }

  /**
   * Test {@link RuntimeConfigurable#addText(String)} with {@code data}.
   * <p>
   * Method under test: {@link RuntimeConfigurable#addText(String)}
   */
  @Test
  public void testAddTextWithData3() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    runtimeConfigurable.addText("");

    // Assert that nothing has changed
    assertEquals("", runtimeConfigurable.getText().toString());
  }

  /**
   * Test {@link RuntimeConfigurable#getText()}.
   * <ul>
   *   <li>Then return toString is {@code Data}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#getText()}
   */
  @Test
  public void testGetText_thenReturnToStringIsData() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.addText("Data");

    // Act and Assert
    assertEquals("Data", runtimeConfigurable.getText().toString());
  }

  /**
   * Test {@link RuntimeConfigurable#getText()}.
   * <ul>
   *   <li>Then return toString is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#getText()}
   */
  @Test
  public void testGetText_thenReturnToStringIsEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new RuntimeConfigurable("Proxy", "Element Tag")).getText().toString());
  }

  /**
   * Test {@link RuntimeConfigurable#maybeConfigure(Project, boolean)} with {@code p}, {@code configureChildren}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#maybeConfigure(Project, boolean)}
   */
  @Test
  public void testMaybeConfigureWithPConfigureChildren_thenThrowBuildException() throws BuildException {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.setAttribute(ComponentHelper.COMPONENT_HELPER_REFERENCE, "42");
    runtimeConfigurable.addText("Data");

    // Act and Assert
    assertThrows(BuildException.class, () -> runtimeConfigurable.maybeConfigure(new Project(), true));
  }

  /**
   * Test {@link RuntimeConfigurable#maybeConfigure(Project)} with {@code p}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#maybeConfigure(Project)}
   */
  @Test
  public void testMaybeConfigureWithP_thenThrowBuildException() throws BuildException {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.setAttribute(ComponentHelper.COMPONENT_HELPER_REFERENCE, "42");
    runtimeConfigurable.addText("Data");

    // Act and Assert
    assertThrows(BuildException.class, () -> runtimeConfigurable.maybeConfigure(new Project()));
  }

  /**
   * Test {@link RuntimeConfigurable#reconfigure(Project)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RuntimeConfigurable#reconfigure(Project)}
   */
  @Test
  public void testReconfigure_thenThrowBuildException() {
    // Arrange
    RuntimeConfigurable runtimeConfigurable = new RuntimeConfigurable("Proxy", "Element Tag");
    runtimeConfigurable.setAttribute(ComponentHelper.COMPONENT_HELPER_REFERENCE, "42");
    runtimeConfigurable.addText("Data");

    // Act and Assert
    assertThrows(BuildException.class, () -> runtimeConfigurable.reconfigure(new Project()));
  }
}
