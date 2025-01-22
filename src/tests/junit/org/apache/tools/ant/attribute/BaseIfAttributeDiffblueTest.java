package org.apache.tools.ant.attribute;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.attribute.IfBlankAttribute.Unless;
import org.junit.Test;

public class BaseIfAttributeDiffblueTest {
  /**
   * Test {@link BaseIfAttribute#isPositive()}.
   * <ul>
   *   <li>Given {@link IfBlankAttribute} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#isPositive()}
   */
  @Test
  public void testIsPositive_givenIfBlankAttribute_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new IfBlankAttribute()).isPositive());
  }

  /**
   * Test {@link BaseIfAttribute#isPositive()}.
   * <ul>
   *   <li>Given {@link Unless} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#isPositive()}
   */
  @Test
  public void testIsPositive_givenUnless_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Unless()).isPositive());
  }

  /**
   * Test {@link BaseIfAttribute#convertResult(boolean)}.
   * <ul>
   *   <li>Given {@link IfBlankAttribute} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#convertResult(boolean)}
   */
  @Test
  public void testConvertResult_givenIfBlankAttribute_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new IfBlankAttribute()).convertResult(true));
  }

  /**
   * Test {@link BaseIfAttribute#convertResult(boolean)}.
   * <ul>
   *   <li>Given {@link Unless} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#convertResult(boolean)}
   */
  @Test
  public void testConvertResult_givenUnless_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Unless()).convertResult(true));
  }

  /**
   * Test {@link BaseIfAttribute#getParams(UnknownElement)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#getParams(UnknownElement)}
   */
  @Test
  public void testGetParams_givenJavaLangObject_thenReturnSizeIsOne() {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");
    wrapper.setAttribute("ant-attribute:param", "42");

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    UnknownElement el = new UnknownElement("Element Name");
    el.setProject(project);
    el.setRuntimeConfigurableWrapper(wrapper);

    // Act
    Map<String, String> actualParams = ifBlankAttribute.getParams(el);

    // Assert
    assertEquals(1, actualParams.size());
    assertEquals("42", actualParams.get("param"));
  }

  /**
   * Test {@link BaseIfAttribute#getParams(UnknownElement)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#getParams(UnknownElement)}
   */
  @Test
  public void testGetParams_givenProjectAddBuildListenerAntClassLoader_thenReturnSizeIsOne() {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");
    wrapper.setAttribute("ant-attribute:param", "42");

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    UnknownElement el = new UnknownElement("Element Name");
    el.setProject(project);
    el.setRuntimeConfigurableWrapper(wrapper);

    // Act
    Map<String, String> actualParams = ifBlankAttribute.getParams(el);

    // Assert
    assertEquals(1, actualParams.size());
    assertEquals("42", actualParams.get("param"));
  }

  /**
   * Test {@link BaseIfAttribute#getParams(UnknownElement)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#getParams(UnknownElement)}
   */
  @Test
  public void testGetParams_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");
    wrapper.setAttribute("ant-attribute:param", "42");

    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    UnknownElement el = new UnknownElement("Element Name");
    el.setProject(project);
    el.setRuntimeConfigurableWrapper(wrapper);

    // Act
    Map<String, String> actualParams = ifBlankAttribute.getParams(el);

    // Assert
    assertEquals(1, actualParams.size());
    assertEquals("42", actualParams.get("param"));
  }

  /**
   * Test {@link BaseIfAttribute#getParams(UnknownElement)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#getParams(UnknownElement)}
   */
  @Test
  public void testGetParams_givenProject_thenReturnSizeIsOne() {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");
    wrapper.setAttribute("ant-attribute:param", "42");

    UnknownElement el = new UnknownElement("Element Name");
    el.setProject(new Project());
    el.setRuntimeConfigurableWrapper(wrapper);

    // Act
    Map<String, String> actualParams = ifBlankAttribute.getParams(el);

    // Assert
    assertEquals(1, actualParams.size());
    assertEquals("42", actualParams.get("param"));
  }

  /**
   * Test {@link BaseIfAttribute#getParams(UnknownElement)}.
   * <ul>
   *   <li>Given {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)} with {@code Proxy} and {@code Element Tag} Attribute {@code Name} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#getParams(UnknownElement)}
   */
  @Test
  public void testGetParams_givenRuntimeConfigurableWithProxyAndElementTagAttributeNameIs42() {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");
    wrapper.setAttribute("Name", "42");

    UnknownElement el = new UnknownElement("Element Name");
    el.setRuntimeConfigurableWrapper(wrapper);

    // Act and Assert
    assertTrue(ifBlankAttribute.getParams(el).isEmpty());
  }

  /**
   * Test {@link BaseIfAttribute#getParams(UnknownElement)}.
   * <ul>
   *   <li>Given {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)} with {@code Proxy} and {@code Element Tag}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseIfAttribute#getParams(UnknownElement)}
   */
  @Test
  public void testGetParams_givenRuntimeConfigurableWithProxyAndElementTag_thenReturnEmpty() {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    UnknownElement el = new UnknownElement("Element Name");
    el.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "Element Tag"));

    // Act and Assert
    assertTrue(ifBlankAttribute.getParams(el).isEmpty());
  }
}
