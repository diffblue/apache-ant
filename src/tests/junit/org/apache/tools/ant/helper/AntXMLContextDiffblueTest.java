package org.apache.tools.ant.helper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Target;
import org.junit.Test;
import org.xml.sax.Locator;
import org.xml.sax.ext.Locator2Impl;

public class AntXMLContextDiffblueTest {
  /**
   * Test {@link AntXMLContext#AntXMLContext(Project)}.
   * <p>
   * Method under test: {@link AntXMLContext#AntXMLContext(Project)}
   */
  @Test
  public void testNewAntXMLContext() {
    // Arrange
    Project project = new Project();

    // Act
    AntXMLContext actualAntXMLContext = new AntXMLContext(project);

    // Assert
    assertNull(actualAntXMLContext.getBuildFile());
    assertNull(actualAntXMLContext.getBuildFileParent());
    assertNull(actualAntXMLContext.getCurrentProjectName());
    assertNull(actualAntXMLContext.getBuildFileParentURL());
    assertNull(actualAntXMLContext.getBuildFileURL());
    assertNull(actualAntXMLContext.getCurrentTargets());
    assertNull(actualAntXMLContext.getCurrentTarget());
    assertNull(actualAntXMLContext.getLocator());
    assertEquals(1, actualAntXMLContext.getTargets().size());
    assertFalse(actualAntXMLContext.isIgnoringProjectTag());
    assertTrue(actualAntXMLContext.getWrapperStack().isEmpty());
    assertSame(project, actualAntXMLContext.getProject());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link AntXMLContext#setCurrentProjectName(String)}
   *   <li>{@link AntXMLContext#setCurrentTarget(Target)}
   *   <li>{@link AntXMLContext#setCurrentTargets(Map)}
   *   <li>{@link AntXMLContext#setIgnoreProjectTag(boolean)}
   *   <li>{@link AntXMLContext#setImplicitTarget(Target)}
   *   <li>{@link AntXMLContext#setLocator(Locator)}
   *   <li>{@link AntXMLContext#getBuildFile()}
   *   <li>{@link AntXMLContext#getBuildFileParent()}
   *   <li>{@link AntXMLContext#getBuildFileParentURL()}
   *   <li>{@link AntXMLContext#getBuildFileURL()}
   *   <li>{@link AntXMLContext#getCurrentProjectName()}
   *   <li>{@link AntXMLContext#getCurrentTarget()}
   *   <li>{@link AntXMLContext#getCurrentTargets()}
   *   <li>{@link AntXMLContext#getImplicitTarget()}
   *   <li>{@link AntXMLContext#getLocator()}
   *   <li>{@link AntXMLContext#getProject()}
   *   <li>{@link AntXMLContext#getTargets()}
   *   <li>{@link AntXMLContext#getWrapperStack()}
   *   <li>{@link AntXMLContext#isIgnoringProjectTag()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Project project = new Project();
    AntXMLContext antXMLContext = new AntXMLContext(project);

    // Act
    antXMLContext.setCurrentProjectName("Name");
    Target target = new Target();
    antXMLContext.setCurrentTarget(target);
    HashMap<String, Target> currentTargets = new HashMap<>();
    antXMLContext.setCurrentTargets(currentTargets);
    antXMLContext.setIgnoreProjectTag(true);
    Target target2 = new Target();
    antXMLContext.setImplicitTarget(target2);
    Locator2Impl locator = new Locator2Impl();
    antXMLContext.setLocator(locator);
    File actualBuildFile = antXMLContext.getBuildFile();
    File actualBuildFileParent = antXMLContext.getBuildFileParent();
    URL actualBuildFileParentURL = antXMLContext.getBuildFileParentURL();
    URL actualBuildFileURL = antXMLContext.getBuildFileURL();
    String actualCurrentProjectName = antXMLContext.getCurrentProjectName();
    Target actualCurrentTarget = antXMLContext.getCurrentTarget();
    Map<String, Target> actualCurrentTargets = antXMLContext.getCurrentTargets();
    Target actualImplicitTarget = antXMLContext.getImplicitTarget();
    Locator actualLocator = antXMLContext.getLocator();
    Project actualProject = antXMLContext.getProject();
    Vector<Target> actualTargets = antXMLContext.getTargets();
    Vector<RuntimeConfigurable> actualWrapperStack = antXMLContext.getWrapperStack();
    boolean actualIsIgnoringProjectTagResult = antXMLContext.isIgnoringProjectTag();

    // Assert
    assertEquals(1, actualTargets.size());
    Target getResult = actualTargets.get(0);
    assertEquals("", getResult.getName());
    assertEquals("", getResult.toString());
    assertEquals("Name", actualCurrentProjectName);
    assertNull(actualBuildFile);
    assertNull(actualBuildFileParent);
    assertNull(getResult.getDescription());
    assertNull(getResult.getIf());
    assertNull(getResult.getUnless());
    assertNull(actualBuildFileParentURL);
    assertNull(actualBuildFileURL);
    assertEquals(0, getResult.getTasks().length);
    assertTrue(actualCurrentTargets.isEmpty());
    assertTrue(actualWrapperStack.isEmpty());
    assertTrue(actualIsIgnoringProjectTagResult);
    assertSame(currentTargets, actualCurrentTargets);
    assertSame(project, getResult.getProject());
    assertSame(project, actualProject);
    assertSame(target, actualCurrentTarget);
    assertSame(target2, actualImplicitTarget);
    assertSame(locator, actualLocator);
  }

  /**
   * Test {@link AntXMLContext#currentWrapper()}.
   * <p>
   * Method under test: {@link AntXMLContext#currentWrapper()}
   */
  @Test
  public void testCurrentWrapper() {
    // Arrange, Act and Assert
    assertNull((new AntXMLContext(new Project())).currentWrapper());
  }

  /**
   * Test {@link AntXMLContext#parentWrapper()}.
   * <p>
   * Method under test: {@link AntXMLContext#parentWrapper()}
   */
  @Test
  public void testParentWrapper() {
    // Arrange, Act and Assert
    assertNull((new AntXMLContext(new Project())).parentWrapper());
  }

  /**
   * Test {@link AntXMLContext#pushWrapper(RuntimeConfigurable)}.
   * <p>
   * Method under test: {@link AntXMLContext#pushWrapper(RuntimeConfigurable)}
   */
  @Test
  public void testPushWrapper() {
    // Arrange
    AntXMLContext antXMLContext = new AntXMLContext(new Project());
    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    antXMLContext.pushWrapper(wrapper);

    // Assert
    Vector<RuntimeConfigurable> wrapperStack = antXMLContext.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    assertSame(wrapper, wrapperStack.get(0));
  }

  /**
   * Test {@link AntXMLContext#addTarget(Target)}.
   * <p>
   * Method under test: {@link AntXMLContext#addTarget(Target)}
   */
  @Test
  public void testAddTarget() {
    // Arrange
    AntXMLContext antXMLContext = new AntXMLContext(new Project());
    Target target = new Target();

    // Act
    antXMLContext.addTarget(target);

    // Assert
    Vector<Target> targets = antXMLContext.getTargets();
    assertEquals(2, targets.size());
    assertSame(target, targets.get(1));
    assertSame(target, antXMLContext.getCurrentTarget());
  }

  /**
   * Test {@link AntXMLContext#getPrefixMapping(String)}.
   * <p>
   * Method under test: {@link AntXMLContext#getPrefixMapping(String)}
   */
  @Test
  public void testGetPrefixMapping() {
    // Arrange, Act and Assert
    assertNull((new AntXMLContext(new Project())).getPrefixMapping("Prefix"));
  }
}
