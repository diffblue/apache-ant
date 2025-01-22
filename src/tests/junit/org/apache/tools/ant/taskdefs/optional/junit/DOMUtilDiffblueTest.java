package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.List;
import javax.imageio.metadata.IIOMetadataNode;
import org.apache.tools.ant.taskdefs.optional.junit.DOMUtil.NodeFilter;
import org.apache.tools.ant.taskdefs.optional.junit.DOMUtil.NodeListImpl;
import org.apache.tools.ant.taskdefs.optional.junit.DOMUtilTest.FooNodeFilter;
import org.junit.Test;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class DOMUtilDiffblueTest {
  /**
   * Test {@link DOMUtil#listChildNodes(Node, NodeFilter, boolean)}.
   * <ul>
   *   <li>Given {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#listChildNodes(Node, NodeFilter, boolean)}
   */
  @Test
  public void testListChildNodes_givenIIOMetadataNodeWith42() {
    // Arrange
    IIOMetadataNode parent = new IIOMetadataNode("foo");
    parent.appendChild(new IIOMetadataNode("42"));

    // Act
    NodeList actualListChildNodesResult = DOMUtil.listChildNodes(parent, (new DOMUtilTest()).new FooNodeFilter(), true);

    // Assert
    assertTrue(actualListChildNodesResult instanceof List);
    assertTrue(((List<Object>) actualListChildNodesResult).isEmpty());
  }

  /**
   * Test {@link DOMUtil#listChildNodes(Node, NodeFilter, boolean)}.
   * <ul>
   *   <li>Given {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code 42}.</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#listChildNodes(Node, NodeFilter, boolean)}
   */
  @Test
  public void testListChildNodes_givenIIOMetadataNodeWith42_whenFalse_thenReturnList() {
    // Arrange
    IIOMetadataNode parent = new IIOMetadataNode("foo");
    parent.appendChild(new IIOMetadataNode("42"));

    // Act
    NodeList actualListChildNodesResult = DOMUtil.listChildNodes(parent, (new DOMUtilTest()).new FooNodeFilter(),
        false);

    // Assert
    assertTrue(actualListChildNodesResult instanceof List);
    assertTrue(((List<Object>) actualListChildNodesResult).isEmpty());
  }

  /**
   * Test {@link DOMUtil#listChildNodes(Node, NodeFilter, boolean)}.
   * <ul>
   *   <li>When {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code foo}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#listChildNodes(Node, NodeFilter, boolean)}
   */
  @Test
  public void testListChildNodes_whenIIOMetadataNodeWithFoo_thenReturnList() {
    // Arrange
    IIOMetadataNode parent = new IIOMetadataNode("foo");

    // Act
    NodeList actualListChildNodesResult = DOMUtil.listChildNodes(parent, (new DOMUtilTest()).new FooNodeFilter(), true);

    // Assert
    assertTrue(actualListChildNodesResult instanceof List);
    assertTrue(((List<Object>) actualListChildNodesResult).isEmpty());
  }

  /**
   * Test {@link DOMUtil#getNodeAttribute(Node, String)}.
   * <ul>
   *   <li>When {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code foo}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#getNodeAttribute(Node, String)}
   */
  @Test
  public void testGetNodeAttribute_whenIIOMetadataNodeWithFoo_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", DOMUtil.getNodeAttribute(new IIOMetadataNode("foo"), "Name"));
  }

  /**
   * Test {@link DOMUtil#getNodeAttribute(Node, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#getNodeAttribute(Node, String)}
   */
  @Test
  public void testGetNodeAttribute_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(DOMUtil.getNodeAttribute(null, "Name"));
  }

  /**
   * Test {@link DOMUtil#getChildByTagName(Node, String)}.
   * <p>
   * Method under test: {@link DOMUtil#getChildByTagName(Node, String)}
   */
  @Test
  public void testGetChildByTagName() {
    // Arrange
    IIOMetadataNode parent = new IIOMetadataNode("foo");
    parent.appendChild(new IIOMetadataNode("foo"));
    IIOMetadataNode iioMetadataNode = new IIOMetadataNode("foo");
    parent.insertBefore(iioMetadataNode, new IIOMetadataNode("foo"));

    // Act and Assert
    assertNull(DOMUtil.getChildByTagName(parent, "Tagname"));
  }

  /**
   * Test {@link DOMUtil#getChildByTagName(Node, String)}.
   * <ul>
   *   <li>Given {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code foo}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#getChildByTagName(Node, String)}
   */
  @Test
  public void testGetChildByTagName_givenIIOMetadataNodeWithFoo_thenReturnNull() {
    // Arrange
    IIOMetadataNode parent = new IIOMetadataNode("foo");
    IIOMetadataNode iioMetadataNode = new IIOMetadataNode("foo");
    parent.insertBefore(iioMetadataNode, new IIOMetadataNode("foo"));

    // Act and Assert
    assertNull(DOMUtil.getChildByTagName(parent, "Tagname"));
  }

  /**
   * Test {@link DOMUtil#getChildByTagName(Node, String)}.
   * <ul>
   *   <li>Then ParentNode return {@link IIOMetadataNode}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#getChildByTagName(Node, String)}
   */
  @Test
  public void testGetChildByTagName_thenParentNodeReturnIIOMetadataNode() throws DOMException {
    // Arrange
    IIOMetadataNode parent = new IIOMetadataNode("foo");
    parent.appendChild(new IIOMetadataNode("Tagname"));
    IIOMetadataNode iioMetadataNode = new IIOMetadataNode("foo");
    parent.insertBefore(iioMetadataNode, new IIOMetadataNode("foo"));

    // Act
    Element actualChildByTagName = DOMUtil.getChildByTagName(parent, "Tagname");

    // Assert
    Node parentNode = actualChildByTagName.getParentNode();
    assertTrue(parentNode instanceof IIOMetadataNode);
    assertTrue(actualChildByTagName instanceof IIOMetadataNode);
    assertEquals("Tagname", actualChildByTagName.getTagName());
    assertEquals("Tagname", actualChildByTagName.getLocalName());
    assertEquals("Tagname", actualChildByTagName.getNodeName());
    assertNull(((IIOMetadataNode) actualChildByTagName).getUserObject());
    assertNull(actualChildByTagName.getNamespaceURI());
    assertNull(actualChildByTagName.getNodeValue());
    assertNull(actualChildByTagName.getPrefix());
    assertNull(actualChildByTagName.getOwnerDocument());
    assertNull(actualChildByTagName.getFirstChild());
    assertNull(actualChildByTagName.getLastChild());
    assertNull(actualChildByTagName.getNextSibling());
    assertNull(actualChildByTagName.getPreviousSibling());
    assertEquals(0, ((IIOMetadataNode) actualChildByTagName).getLength());
    assertEquals((short) 1, actualChildByTagName.getNodeType());
    assertFalse(actualChildByTagName.hasAttributes());
    assertFalse(actualChildByTagName.hasChildNodes());
    assertSame(parent, parentNode);
  }

  /**
   * Test {@link DOMUtil#getChildByTagName(Node, String)}.
   * <ul>
   *   <li>When {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code foo}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#getChildByTagName(Node, String)}
   */
  @Test
  public void testGetChildByTagName_whenIIOMetadataNodeWithFoo_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(DOMUtil.getChildByTagName(new IIOMetadataNode("foo"), "Tagname"));
  }

  /**
   * Test {@link DOMUtil#getChildByTagName(Node, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMUtil#getChildByTagName(Node, String)}
   */
  @Test
  public void testGetChildByTagName_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(DOMUtil.getChildByTagName(null, "Tagname"));
  }

  /**
   * Test NodeListImpl {@link NodeListImpl#getLength()}.
   * <p>
   * Method under test: {@link NodeListImpl#getLength()}
   */
  @Test
  public void testNodeListImplGetLength() {
    // Arrange, Act and Assert
    assertEquals(0, (new NodeListImpl()).getLength());
  }

  /**
   * Test NodeListImpl {@link NodeListImpl#item(int)}.
   * <ul>
   *   <li>Given {@link NodeListImpl} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NodeListImpl#item(int)}
   */
  @Test
  public void testNodeListImplItem_givenNodeListImpl_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new NodeListImpl()).item(1));
  }

  /**
   * Test NodeListImpl {@link NodeListImpl#item(int)}.
   * <ul>
   *   <li>Then return {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NodeListImpl#item(int)}
   */
  @Test
  public void testNodeListImplItem_thenReturnIIOMetadataNodeWithFoo() {
    // Arrange
    NodeListImpl nodeListImpl = new NodeListImpl();
    nodeListImpl.add(new IIOMetadataNode("foo"));
    IIOMetadataNode iioMetadataNode = new IIOMetadataNode("foo");
    nodeListImpl.add(iioMetadataNode);

    // Act and Assert
    assertSame(iioMetadataNode, nodeListImpl.item(1));
  }

  /**
   * Test NodeListImpl new {@link NodeListImpl} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link NodeListImpl}
   */
  @Test
  public void testNodeListImplNewNodeListImpl() {
    // Arrange, Act and Assert
    assertTrue((new NodeListImpl()).isEmpty());
  }
}
